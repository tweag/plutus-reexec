module PSR.Storage.SQLite (withSqliteStorage) where

import Cardano.Api (
    BlockHeader (..),
    ScriptHash (..),
 )
import Cardano.Ledger.Binary (mkVersion64)
import Cardano.Ledger.Binary qualified as L
import Cardano.Ledger.Plutus (CostModel, ExUnits (..), encodeCostModel)
import Data.Functor (void, (<&>))
import Data.Maybe (mapMaybe)
import Data.Pool (Pool, defaultPoolConfig, newPool, withResource)
import Database.SQLite.Simple hiding (execute, executeNamed, query, queryNamed)
import PSR.Events.Interface
import PSR.Storage.Interface
import PSR.Storage.SQLite.GetEvents qualified as GetEvents
import PSR.Storage.SQLite.Instances ()
import PSR.Storage.SQLite.Metrics (SqliteMetrics (..), initialiseMetrics)
import PSR.Storage.SQLite.Utils
import PlutusLedgerApi.Common (MajorProtocolVersion (..))

withSqliteStorage :: FilePath -> (Storage -> IO ()) -> IO ()
withSqliteStorage dbPath act = do
    pool <- newPool (defaultPoolConfig (openWithPragmas dbPath) close 120 10)
    metrics <- initialiseMetrics
    storage <- mkStorage metrics pool
    act storage

mkStorage :: SqliteMetrics -> Pool Connection -> IO Storage
mkStorage metrics pool = do
    withResource pool initSchema
    pure $ Storage{..}
  where
    getEvents = GetEvents.getEvents metrics.getEvents_select pool

    createBlockIfNotExists :: Connection -> BlockHeader -> IO ()
    createBlockIfNotExists conn (BlockHeader slotNo hash blockNo) = do
        execute
            metrics.createBlockIfNotExists_insert
            conn
            "INSERT OR IGNORE INTO block (block_no, slot_no, hash) values (?, ?, ?)"
            (blockNo, slotNo, hash)

    getOrCreateCostModelParamsId :: Connection -> MajorProtocolVersion -> CostModel -> IO Integer
    getOrCreateCostModelParamsId conn (MajorProtocolVersion v) costModel = do
        version <- mkVersion64 $ fromIntegral v
        let params = L.serialize version $ encodeCostModel costModel
        execute
            metrics.getOrCreateCostModelParamsId_insert
            conn
            "INSERT OR IGNORE INTO cost_model_params (params) values (?)"
            (Only params)

        rows <-
            query
                metrics.getOrCreateCostModelParamsId_select
                conn
                "SELECT params_id from cost_model_params where params = ?"
                (Only params)
        case rows of
            [Only paramsId :: Only Integer] -> return paramsId
            _ -> error "Can't find the inserted block"

    addExecutionEvent :: ExecutionContextId -> TraceLogs -> Maybe EvalError -> ExUnits -> IO ()
    addExecutionEvent eci logs evalError exUnits =
        withResource pool $ \conn -> withTransaction conn $ do
            let
                ExUnits{exUnitsMem, exUnitsSteps} = exUnits
                params =
                    [ ":context_id" := eci
                    , ":trace_logs" := logs
                    , ":eval_error" := evalError
                    , ":exec_budget_cpu" := toInteger exUnitsSteps
                    , ":exec_budget_mem" := toInteger exUnitsMem
                    ]
            executeNamed
                metrics.addExecutionEvent_insert
                conn
                "INSERT INTO execution_event \
                \ (context_id, trace_logs, eval_error, exec_budget_cpu, exec_budget_mem) \
                \ values (:context_id, :trace_logs, :eval_error, :exec_budget_cpu, :exec_budget_mem)"
                params

    addExecutionContext :: BlockHeader -> ExecutionContext -> IO ExecutionContextId
    addExecutionContext blockHeader@(BlockHeader _ hash _) ExecutionContext{..} =
        withResource pool $ \conn -> withTransaction conn $ do
            void $ createBlockIfNotExists conn blockHeader
            costModelParamsId <- getOrCreateCostModelParamsId conn majorProtocolVersion costModel
            let
                ExUnits exBudgetMaxCpu exBudgetMaxMem = exMaxBudget
                params =
                    [ ":block_hash" := hash
                    , ":transaction_hash" := transactionHash
                    , ":script_hash" := scriptHash
                    , ":script_name" := scriptName
                    , ":ledger_language" := ledgerLanguage
                    , ":major_protocol_version" := majorProtocolVersion
                    , ":datum" := datum
                    , ":redeemer" := redeemer
                    , ":script_context" := scriptContext
                    , ":exec_budget_max_cpu" := toInteger exBudgetMaxCpu
                    , ":exec_budget_max_mem" := toInteger exBudgetMaxMem
                    , ":cost_model_params_id" := costModelParamsId
                    ]
            rows <-
                queryNamed
                    metrics.addExecutionEvent_insert
                    conn
                    "INSERT INTO execution_context \
                    \ (block_hash, \
                    \ transaction_hash, \
                    \ script_hash, \
                    \ script_name, \
                    \ ledger_language, \
                    \ major_protocol_version, \
                    \ datum, \
                    \ redeemer, \
                    \ script_context, \
                    \ exec_budget_max_cpu, \
                    \ exec_budget_max_mem, \
                    \ cost_model_params_id) \
                    \ VALUES \
                    \ (:block_hash, \
                    \ :transaction_hash, \
                    \ :script_hash, \
                    \ :script_name, \
                    \ :ledger_language, \
                    \ :major_protocol_version, \
                    \ :datum, \
                    \ :redeemer, \
                    \ :script_context, \
                    \ :exec_budget_max_cpu, \
                    \ :exec_budget_max_mem, \
                    \ :cost_model_params_id) \
                    \ RETURNING context_id"
                    params
            case rows of
                [(Only cei) :: Only ExecutionContextId] -> pure cei
                _ ->
                    -- TODO: handle the error properly
                    error "Failed to return execution context id"

    addCancellationEvent :: BlockHeader -> ScriptHash -> IO ()
    addCancellationEvent blockHeader@(BlockHeader _ hash _) scriptHash =
        withResource pool $ \conn -> withTransaction conn $ do
            void $ createBlockIfNotExists conn blockHeader
            execute
                metrics.addCancellationEvent_insert
                conn
                "INSERT INTO cancellation_event (block_hash, script_hash) values (?, ?)"
                (hash, scriptHash)

    addSelectionEvent :: BlockHeader -> IO ()
    addSelectionEvent blockHeader@(BlockHeader _ hash _) =
        withResource pool $ \conn -> withTransaction conn $ do
            void $ createBlockIfNotExists conn blockHeader
            execute
                metrics.addSelectionEvent_insert
                conn
                "INSERT INTO selection_event (block_hash) values (?)"
                (Only hash)

    getExecutionContexts :: [FilterBy] -> IO [(BlockHeader, ExecutionContextId, ExecutionContext)]
    getExecutionContexts filters =
        withResource pool $ \conn -> withTransaction conn $ do
            let
                (whereQuery, whereParams) =
                    mkWhereWithParams $
                        filters <&> \case
                            ByNameOrHash scriptNameOrHash ->
                                ( " (HEX(ec.script_hash) = UPPER(:name_or_hash) OR ec.script_name = :name_or_hash) "
                                , ":name_or_hash" := scriptNameOrHash
                                )
                            ByTxId txId ->
                                ( " (HEX(ec.transaction_hash) = UPPER(:transaction_hash)) "
                                , ":transaction_hash" := txId
                                )
                            ByContextId cid ->
                                ( " (ec.context_id = :context_id) "
                                , ":context_id" := cid
                                )
                sqlQuery =
                    "SELECT b.slot_no, b.hash, b.block_no, \
                    \ ec.context_id, \
                    \ ec.transaction_hash, \
                    \ ec.script_name, \
                    \ ec.script_hash, \
                    \ ec.ledger_language, \
                    \ ec.major_protocol_version, \
                    \ ec.datum, \
                    \ ec.redeemer, \
                    \ ec.script_context, \
                    \ ec.exec_budget_max_cpu, \
                    \ ec.exec_budget_max_mem, \
                    \ cmp.params \
                    \ FROM execution_context ec \
                    \ LEFT JOIN block b ON ec.block_hash = b.hash \
                    \ LEFT JOIN cost_model_params cmp ON cmp.params_id = ec.cost_model_params_id "
                        <> whereQuery
                        <> " ORDER BY ec.created_at ASC LIMIT 1"

            rows :: [BlockHeader :. Only ExecutionContextId :. Maybe ExecutionContext] <-
                queryNamed metrics.getExecutionContextByNameOrScriptHash_select conn sqlQuery whereParams

            pure $
                mapMaybe
                    ( \case
                        (bh :. Only eci :. Just ec) -> Just (bh, eci, ec)
                        _ -> Nothing
                    )
                    rows
