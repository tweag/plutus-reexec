module PSR.Types (
    ChainSyncEvent (..),
    ChainSyncEventException (..),
    Block (..),
) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Cardano.Api qualified as C
import Control.Exception (Exception)
import GHC.Generics (Generic)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

data ChainSyncEvent
    = RollForward C.BlockInMode C.ChainTip
    | RollBackward C.ChainPoint C.ChainTip
    deriving stock (Show, Generic)

data ChainSyncEventException = NoIntersectionFound
    deriving stock (Show)
    deriving anyclass (Exception)

data Block where
    Block :: C.ShelleyBasedEra era -> [C.Tx era] -> Block

deriving instance Show Block

