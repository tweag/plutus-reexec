module PSR.Events where

import Control.Concurrent.STM.TChan (newBroadcastTChanIO, writeTChan)
import Control.Monad.STM qualified as STM
import Data.Foldable (for_)
import Data.Time (getCurrentTime)
import PSR.Events.Interface
import PSR.Storage.Interface (Storage (..))

withEvents :: Maybe Storage -> (Events -> IO ()) -> IO ()
withEvents maybeStorage act = do
    eventsChannel <- newBroadcastTChanIO

    let
        getEventsChannel = eventsChannel

    let
        addCancellationEvent blockHeader scriptHash = do
            createdAt <- getCurrentTime
            STM.atomically $
                writeTChan eventsChannel $
                    Event
                        { eventType = Cancellation
                        , blockHeader
                        , createdAt
                        , payload = CancellationPayload scriptHash
                        }

            for_ maybeStorage $ \s ->
                s.addCancellationEvent blockHeader scriptHash

    let
        addSelectionEvent blockHeader = do
            createdAt <- getCurrentTime
            STM.atomically $
                writeTChan eventsChannel $
                    Event
                        { eventType = Selection
                        , blockHeader
                        , createdAt
                        , payload = SelectionPayload
                        }
            for_ maybeStorage $ \s ->
                s.addSelectionEvent blockHeader

    let
        addExecutionEvent blockHeader executionContextId payload@ExecutionEventPayload{..} = do
            createdAt <- getCurrentTime

            for_ maybeStorage $ \s ->
                s.addExecutionEvent executionContextId traceLogs evalError exUnits

            let event =
                    Event
                        { eventType = Execution
                        , blockHeader
                        , createdAt
                        , payload = ExecutionPayload payload
                        }
            STM.atomically $ writeTChan eventsChannel event
            pure event

    let
        addExecutionContext bh ec = case maybeStorage of
            Nothing -> pure $ ExecutionContextId 0 -- in case of no storage we return the null id because it will be ignored
            Just s -> s.addExecutionContext bh ec

    act $ Events{..}
