module PSR.Events.FileLogger (run) where

import Control.Concurrent.STM.TChan (dupTChan, readTChan)
import Control.Monad (forever)
import Control.Monad.STM qualified as STM
import Data.Aeson qualified as Aeson
import Data.ByteString as BS
import System.IO (IOMode (AppendMode), hFlush, withBinaryFile)

import PSR.Events.Interface (Events (..))
import PSR.HTTP.API ()

run :: Events -> FilePath -> IO ()
run events logsPath = do
    putStrLn $ "Writing events logs to " <> logsPath
    inputChannel <- STM.atomically $ dupTChan events.getEventsChannel
    withBinaryFile logsPath AppendMode $ \handle -> forever $ do
        event <- STM.atomically $ readTChan inputChannel
        BS.hPut handle $ BS.toStrict (Aeson.encode event) <> "\n"
        hFlush handle
