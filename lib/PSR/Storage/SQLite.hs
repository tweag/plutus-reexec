{-# OPTIONS_GHC -Wno-orphans #-}

module PSR.Storage.SQLite where

import PSR.Storage.Interface 

import Cardano.Api (
  BlockHeader(..),
  BlockNo(..),
  SlotNo(..),
  Hash(..),
  ScriptHash(..),
  TxId,
  serialiseToRawBytes,
 )

import Database.SQLite.Simple
import Database.SQLite.Simple.ToField

withSqliteStorage :: FilePath -> (Storage -> IO ()) -> IO () 
withSqliteStorage dbPath act =  
  withConnection dbPath $ \sqliteConn -> do
    storage <- mkStorage sqliteConn 
    act storage

mkStorage :: Connection -> IO Storage 
mkStorage conn = do
  initSchema conn

  let
    addExecutionEvent :: ExecutionEventPayload -> IO ()
    addExecutionEvent ExecutionEventPayload{..} = do 
      let (BlockHeader slotNo hash blockNo) = blockHeader 

      withTransaction conn $ do
        execute conn
          "INSERT OR IGNORE INTO block (block_no, slot_no, hash) values (?, ?, ?)"
          (blockNo, slotNo, hash)

        execute conn
          "INSERT INTO execution_event (block_no, transaction_hash, script_hash, name, trace) values (?, ?, ?, ?, ?)"
          (blockNo, transactionHash, scriptHash, scriptName, trace)

  let
    addCancellationEvent :: BlockHeader -> ScriptHash -> IO ()
    addCancellationEvent (BlockHeader slotNo hash blockNo) scriptHash = 
      withTransaction conn $ do
        execute conn
          "INSERT OR IGNORE INTO block (block_no, slot_no, hash) values (?, ?, ?)"
          (blockNo, slotNo, hash)

        execute conn
          "INSERT INTO cancellation_event (block_no, script_hash) values (?, ?)"
          (blockNo, scriptHash)
    
  let
    addSelectionEvent :: BlockHeader -> IO ()
    addSelectionEvent (BlockHeader slotNo hash blockNo) =  
      withTransaction conn $ do
        execute conn
          "INSERT OR IGNORE INTO block (block_no, slot_no, hash) values (?, ?, ?)"
          (blockNo, slotNo, hash)

        execute conn
          "INSERT INTO selection_event (block_no) values (?)"
          (Only blockNo)

  let
    getEvents :: FilterQueryParams -> IO [Event] 
    getEvents = undefined
    
  pure $ Storage {..}

initSchema :: Connection -> IO ()
initSchema conn = withTransaction conn $ do
  execute_ conn 
    "CREATE TABLE IF NOT EXISTS block( \
    \ block_no UNSIGNED BIGINT NOT NULL PRIMARY KEY, \
    \ slot_no UNSIGNED BIGINT NOT NULL UNIQUE, \
    \ hash BLOB NOT NULL UNIQUE)"
    
  execute_ conn
    "CREATE TABLE IF NOT EXISTS execution_event(\
    \ block_no UNSIGNED BIGINT NOT NULL REFERENCES block(block_no), \
    \ transaction_hash BLOB NOT NULL, \
    \ script_hash BLOB NOT NULL, \
    \ name TEXT, \
    \ trace TEXT)"

  execute_ conn "CREATE TABLE IF NOT EXISTS cancellation_event (block_number UNSIGNED BIGINT NOT NULL REFERENCES block(block_no), script_hash BLOB NOT NULL)"

  execute_ conn "CREATE TABLE IF NOT EXISTS selection_event (block_no UNSIGNED BIGINT NOT NULL REFERENCES block(block_no))"


-- instances --

instance ToField BlockNo where
  toField (BlockNo blockNo) = toField blockNo

instance ToField SlotNo where
  toField (SlotNo slotNo) = toField slotNo 

instance ToField (Hash BlockHeader) where
  toField hash = toField $ serialiseToRawBytes hash 

instance ToField ScriptHash where
  toField hash = toField $ serialiseToRawBytes hash 

instance ToField TxId where
  toField txId = toField $ serialiseToRawBytes txId 
