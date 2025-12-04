--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Cardano.Api (
    ChainPoint (..),
 )
import Cardano.Api qualified as C
import Control.Monad (unless)
import Data.Function ((&))
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Yaml (decodeFileThrow)
import Options
import Options.Applicative
import PSR.ConfigMap qualified as CM
import PSR.Streaming
import Streamly.Data.Fold.Prelude qualified as Fold
import Streamly.Data.Scanl.Prelude qualified as Scanl
import Streamly.Data.Stream.Prelude qualified as Stream
import System.Exit (exitFailure)

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

-- TODO: Move this into another module.
-- TODO: Make this logic more efficient.
-- NOTE: The idea is to check if the script policy is triggered by either due to
-- Minting or Spending.
shouldTrackTransaction ::
    C.LocalNodeConnectInfo ->
    Map.Map C.PolicyId CM.ScriptDetails ->
    ChainPoint ->
    Transaction ->
    IO Bool
shouldTrackTransaction conn confPolicySet cp (Transaction _ tx) =
    if isUsedInMintingMode
        then pure True
        else isUsedInSpendingMode
  where
    hasIntersectionWithConf :: Set.Set C.PolicyId -> Bool
    hasIntersectionWithConf = not . Map.null . Map.restrictKeys confPolicySet

    isUsedInMintingMode =
        hasIntersectionWithConf $ getPolicySet $ getMintedValue tx

    isUsedInSpendingMode :: IO Bool
    isUsedInSpendingMode = do
        umap <- queryInputUtxoMap conn cp tx
        unless (Map.null umap) $ do
            putStrLn "\nFound UTxO values:"
            mapM_ print $ Map.toList umap
        let valueSetList = getPolicySet . getTxOutValue <$> Map.elems umap
            combinedPolicySet = Set.unions valueSetList
        let foundPolicies = Map.restrictKeys confPolicySet combinedPolicySet
        pure $ not $ Map.null foundPolicies

-- scriptsInTransaction

-- Pairs the current set of transactions with the previous chainpoint
delayedScan ::
    Scanl.Scanl
        IO
        (ChainPoint, [Transaction])
        (ChainPoint, [Transaction])
delayedScan =
    (\(prev, _, trs) -> (prev, trs))
        <$> Scanl.mkScanl
            (\(_, prev, _) (new, tr) -> (prev, new, tr))
            (undefined, undefined, [])

getPolicySet' :: Transaction -> Set.Set C.PolicyId
getPolicySet' (Transaction _ tx) =
    getPolicySet (getMintedValue tx)

isByron :: ChainSyncEvent -> Bool
isByron (RollForward (C.BlockInMode C.ByronEra _) _) = True
isByron _ = False

main :: IO ()
main = do
    Options{..} <- execParser psrOpts
    CM.ConfigMap{..} <- decodeFileThrow scriptYaml
    -- TODO: Remove
    -- TODO: Is there really no better way to parse a hash???
    hash <- case C.deserialiseFromJSON "\"082efb838a6a38c435bf5a1c823569ad224fab42034d265b95f88665ee27877f\"" of
        Left err -> do
            print err
            exitFailure
        Right h -> pure h
    let points = maybe [C.ChainPoint 173166134 hash] pure start -- [ChainPointAtGenesis]
    -- TODO: Use a logging interface instead of using putStrLn.
    putStrLn "Started..."

    let confPolicyMap = Map.fromList [(CM.script_hash x, x) | x <- scripts]
        conn = mkLocalNodeConnectInfo networkId socketPath
    streamChainSyncEvents conn points -- Stream m ChainSyncEvent
    -- TODO: Try to replace "concatMap" with "unfoldEach".
        & Stream.filter (not . isByron)
        & fmap getEventTransactions
        & Stream.postscanl delayedScan
        & Stream.concatMap
            (Stream.fromList . (\(a, b) -> (a,) <$> b))
        & Stream.trace
            ( \(_, tx) ->
                let pols = getPolicySet' tx
                 in unless (Set.null pols) (print pols)
            )
        & Stream.filterM (uncurry (shouldTrackTransaction conn confPolicyMap))
        & Stream.fold (Fold.drainMapM (printIfKnown confPolicyMap))

-- & Stream.fold (Fold.drainMapM (const (pure ())))

printIfKnown :: Map.Map C.PolicyId CM.ScriptDetails -> (ChainPoint, Transaction) -> IO ()
printIfKnown policies (_, tr) = mapM_ print res
  where
    res = Map.restrictKeys policies (getPolicySet' tr)
