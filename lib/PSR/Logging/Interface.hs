{-# LANGUAGE RankNTypes #-}

module PSR.Logging.Interface (
    LogSeverity (..),
    Logger (..),
    HasLogger (..),
    logMsgR,
    logMsgWithR,
) where

import Control.Monad.Reader (MonadIO (..), MonadReader, asks)
import Data.Aeson (ToJSON)
import Data.Text (Text)

data LogSeverity
    = Debug
    | Message
    | Warning
    | Error
    deriving (Show, Eq, Ord)

data Logger = forall env. Logger
    { logAtLevel :: env -> LogSeverity -> Text -> IO ()
    , logAtLevelWith :: forall a. (ToJSON a) => env -> LogSeverity -> Text -> a -> IO ()
    , logEnv :: env
    }

class HasLogger a where getLogger :: a -> Logger
instance HasLogger Logger where getLogger = id

logMsgR ::
    (MonadReader context m, HasLogger context, MonadIO m) =>
    LogSeverity ->
    Text ->
    m ()
logMsgR level message = do
    Logger{..} <- asks getLogger
    liftIO $ logAtLevel logEnv level message

logMsgWithR ::
    (MonadReader context m, HasLogger context, MonadIO m, ToJSON a) =>
    LogSeverity ->
    Text ->
    a ->
    m ()
logMsgWithR level message a = do
    Logger{..} <- asks getLogger
    liftIO $ logAtLevelWith logEnv level message a
