--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Cardano.Api qualified as C
import Control.Concurrent.Async qualified as Async
import Log (LogLevel (..), Logger, LoggerEnv (..))
import Log.Backend.StandardOutput (withJsonStdOutLogger)
import Options
import Options.Applicative
import PSR.ConfigMap qualified as CM
import PSR.HTTP qualified as HTTP
import PSR.Storage.SQLite qualified as Storage
import PSR.Streaming qualified as Streaming
import PSR.Types

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

mkLoggerEnv :: Logger -> LoggerEnv
mkLoggerEnv logger =
    LoggerEnv
        { leLogger = logger
        , leComponent = "PSR"
        , leDomain = []
        , leData = []
        , leMaxLogLevel = LogTrace
        }

main :: IO ()
main = do
    Options{..} <- execParser psrOpts
    config@CM.ConfigMap{..} <- CM.readConfigMap scriptYaml networkId socketPath >>= either error pure

    start <- maybe (C.chainTipToChainPoint <$> C.getLocalChainTip _cmLocalNodeConn) pure _cmStart
    let points = [start]

    withJsonStdOutLogger $ \logger -> do
        let loggerEnv = mkLoggerEnv logger
        Storage.withSqliteStorage sqlitePath $ \storage ->
            Async.withAsync (HTTP.run loggerEnv storage httpServerPort) $ \serverAsync -> do
                Async.link serverAsync

                let appConf = AppConfig loggerEnv config storage
                runApp appConf $ do
                    logAttention_ "Started..."
                    Streaming.mainLoop points
