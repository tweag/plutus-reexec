{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module PSR.Types (
    App (..),
    runApp,
    module Export,
    AppConfig (..),
    HasConfigMap (..),
    ChainSyncEvent (..),
    ChainSyncEventException (..),
    Block (..),
    isByron,
) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Cardano.Api qualified as C
import Control.Exception (Exception)
import Control.Monad.Except (ExceptT (..), MonadError, runExceptT)
import Control.Monad.IO.Unlift
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import GHC.Generics (Generic)
import Lens.Micro.Platform
import Lens.Micro.Platform qualified as Export (view)

import PSR.ConfigMap
import PSR.Logging.Interface (HasLogger (..), Logger)
import PSR.Storage.Interface

--------------------------------------------------------------------------------
-- App context
--------------------------------------------------------------------------------

data AppConfig = AppConfig
    { _confLogger :: Logger
    , _confConfigMap :: ConfigMap
    , _confStorage :: Storage
    }

-- class HasAppConfig a where
--      appConfig :: Lens a AppConfig,
--      confLogger :: Lens a ConfigMap
--      ...
makeClassy ''AppConfig
instance HasConfigMap AppConfig where configMap = confConfigMap
instance HasLogger AppConfig where getLogger = view confLogger

--------------------------------------------------------------------------------
-- App monad
--------------------------------------------------------------------------------

newtype App context err a
    = App {_runApp :: ExceptT err (ReaderT context IO) a}
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadReader context
        , MonadError err
        )

runApp :: context -> App context err a -> IO (Either err a)
runApp conf = flip runReaderT conf . runExceptT . _runApp

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

data ChainSyncEvent
    = RollForward C.BlockInMode C.ChainTip
    | RollBackward C.ChainPoint C.ChainTip
    deriving stock (Show, Generic)

isByron :: ChainSyncEvent -> Bool
isByron (RollForward (C.BlockInMode C.ByronEra _) _) = True
isByron _ = False

data ChainSyncEventException = NoIntersectionFound
    deriving stock (Show)
    deriving anyclass (Exception)

data Block where
    Block :: C.ShelleyBasedEra era -> [C.Tx era] -> Block

deriving instance Show Block
