{-# LANGUAGE RankNTypes #-}

module PSR.ContextBuilder (
    BlockContext (..),
    TransactionContext (..),
    mkBlockContext,
    mkTransactionContext,
) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Cardano.Api qualified as C
import Cardano.Api.Ledger qualified as L
import Cardano.Ledger.Alonzo.Core qualified as L
import Cardano.Ledger.Alonzo.Tx qualified as L
import Cardano.Ledger.Api qualified as L
import Cardano.Ledger.Api.Scripts qualified as S
import Cardano.Ledger.Conway.Tx qualified as L
import Cardano.Ledger.Plutus qualified as L
import Control.Monad (guard)
import Control.Monad.Trans.Writer (WriterT (runWriterT))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import PSR.Chain
import PSR.ConfigMap (ConfigMap (..), ResolvedScript (..), ScriptEvaluationParameters (..))
import PlutusLedgerApi.Common
import PlutusLedgerApi.V1.EvaluationContext qualified as V1
import PlutusLedgerApi.V2.EvaluationContext qualified as V2
import PlutusLedgerApi.V3.EvaluationContext qualified as V3

--------------------------------------------------------------------------------
-- Block Context
--------------------------------------------------------------------------------

-- NOTE: Our decisions are made based on the context built. At different stages
-- of the pipeline the context we are looking at keeps increasing and our
-- decisions are based on that.

-- Q: Is there a better way to represent this?

-- Context1 is essentially acts like the global environment.
data BlockContext era where
    BlockContext ::
        { ctxPrevChainPoint :: C.ChainPoint
        , ctxShelleyBasedEra :: C.ShelleyBasedEra era
        , ctxTransactions :: [C.Tx era]
        , ctxInputUtxoMap :: C.UTxO era
        , -- NOTE: The protocol parameters (and hence the cost models) may change
          -- in a running node. It may be okay to poll this at the block boundary.
          ctxPParams :: L.PParams (C.ShelleyLedgerEra era)
        , ctxEraHistory :: C.EraHistory
        , ctxSysStart :: C.SystemStart
        } ->
        BlockContext era

-- deriving instance Show (BlockContext era)

mkBlockContext ::
    C.LocalNodeConnectInfo ->
    C.ChainPoint ->
    C.ShelleyBasedEra era ->
    [C.Tx era] ->
    IO (BlockContext era)
mkBlockContext conn prevCp era txs = do
    let query =
            BlockContext prevCp era txs
                <$> utxoMapQuery era txs
                <*> pParamsQuery era
                <*> eraHistoryQuery
                <*> sysStartQuery
    -- NOTE: We can catch CostModelsQueryException and choose to retry or skip.
    runLocalStateQueryExpr conn prevCp query

--------------------------------------------------------------------------------
-- Transaction Context
--------------------------------------------------------------------------------

data TransactionContext era where
    TransactionContext ::
        { ctxTransaction :: C.Tx era
        , ctxRelevantScripts :: Map.Map C.ScriptHash ResolvedScript
        } ->
        TransactionContext era

getMintPolicies :: C.Tx era -> Set C.ScriptHash
getMintPolicies =
    Set.mapMonotonic C.unPolicyId
        . getPolicySet
        . C.txMintValueToValue
        . C.txMintValue
        . C.getTxBodyContent
        . C.getTxBody

getInputScriptAddrs ::
    Map C.TxIn (C.TxOut C.CtxUTxO era) -> C.Tx era -> Set C.ScriptHash
getInputScriptAddrs utxoMap tx =
    let utxoList = Map.elems $ Map.restrictKeys utxoMap $ getTxInSet tx
     in Set.fromList $ mapMaybe getTxOutScriptAddr utxoList

getNonEmptyIntersection ::
    ConfigMap ->
    BlockContext era ->
    C.Tx era ->
    Maybe (Map.Map C.ScriptHash ResolvedScript)
getNonEmptyIntersection ConfigMap{..} BlockContext{..} tx = do
    let inpUtxoMap = C.unUTxO ctxInputUtxoMap
        interestingScripts =
            Map.restrictKeys cmScripts $
                Set.union (getMintPolicies tx) (getInputScriptAddrs inpUtxoMap tx)
    guard (not $ Map.null interestingScripts)
    pure $ interestingScripts

makeEvaluationContext ::
    S.CostModels ->
    PlutusLedgerLanguage ->
    C.ExceptT String IO EvaluationContext
makeEvaluationContext params lang = case lang of
    PlutusV1 -> run L.PlutusV1 V1.mkEvaluationContext
    PlutusV2 -> run L.PlutusV2 V2.mkEvaluationContext
    PlutusV3 -> run L.PlutusV3 V3.mkEvaluationContext
  where
    run lng f = case Map.lookup lng (L.costModelsValid params) of
        Just costs -> C.modifyError show . fmap fst . runWriterT $ f (L.getCostModelParams costs)
        Nothing -> C.throwError $ "Unknown cost model for lang: " ++ show lang

mkTransactionContext ::
    ConfigMap -> BlockContext era -> C.Tx era -> IO (Maybe (TransactionContext era))
mkTransactionContext cm bc@BlockContext{..} tx = do
    -- NOTE: convert the era to the script's only eras
    case tx of
        C.ShelleyTx (C.ShelleyBasedEraConway) tx' -> do
            let evalResults =
                    L.evalTxExUnitsWithLogs
                        ctxPParams
                        tx'
                        (C.toLedgerUTxO ctxShelleyBasedEra ctxInputUtxoMap)
                        (C.unLedgerEpochInfo (C.toLedgerEpochInfo ctxEraHistory))
                        ctxSysStart
            print evalResults
        _ -> print ()

    case getNonEmptyIntersection cm bc tx of
        Nothing -> pure Nothing
        Just nei -> pure $ Just $ TransactionContext tx nei
