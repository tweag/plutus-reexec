--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Cardano.Api (ChainPoint(..))
import Data.Function ((&))
import PSR.Streaming (streamChainSyncEvents)
import Streamly.Data.Stream.Prelude qualified as Stream
import Streamly.Data.Fold.Prelude qualified as Fold

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = do
  socketPath <- error "Get socket path from CLI"
  networkId <- error "Get network id from CLI"
  let points = [ChainPointAtGenesis]
  streamChainSyncEvents socketPath networkId points
    & Stream.fold (Fold.drainMapM print)
