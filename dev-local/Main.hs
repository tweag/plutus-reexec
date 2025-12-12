{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

import Control.Concurrent (threadDelay)
import Data.Function ((&))
import Data.Word (Word8)
import Streamly.Data.Array (Array)
import Streamly.Data.Fold qualified as Fold
import Streamly.Data.Stream (Stream)
import Streamly.Data.Stream qualified as Stream
import Streamly.System.Command qualified as Cmd
import Streamly.Unicode.Stream qualified as Unicode
import Streamly.Unicode.String (str)
import System.FilePath ((</>))

-------------------------------------------------------------------------------
-- Utils
-------------------------------------------------------------------------------

printStep :: String -> IO ()
printStep s = putStrLn . unlines $ ["", divider, s, divider]
  where
    divider = replicate 80 '-'

drain :: (Monad m) => Stream m a -> m ()
drain = Stream.fold Fold.drain

nonEmptyLines :: Stream IO (Array Word8) -> Stream IO String
nonEmptyLines inp =
    Unicode.decodeUtf8Chunks inp
        & Stream.foldMany (Fold.takeEndBy_ (== '\n') Fold.toList)
        & Stream.filter (not . null)

firstNonEmptyLine :: String -> Stream IO (Array Word8) -> IO String
firstNonEmptyLine tag =
    Stream.fold (maybe (error [str|Empty: #{tag}|]) id <$> Fold.one)
        . nonEmptyLines

printVar :: String -> String -> IO ()
printVar tag val = putStrLn [str|[#{tag}]: #{val}|]

-------------------------------------------------------------------------------
-- Globals
-------------------------------------------------------------------------------

env_CARDANO_SOCKET :: FilePath
env_CARDANO_SOCKET = "./devnet-env/socket/node1/sock"

env_CARDANO_TESTNET_MAGIC :: Int
env_CARDANO_TESTNET_MAGIC = 42

env_FAUCET_WALLET_SKEY_FILE :: FilePath
env_FAUCET_WALLET_SKEY_FILE = "devnet-env/utxo-keys/utxo1/utxo.skey"

env_FAUCET_WALLET_ADDR :: IO String
env_FAUCET_WALLET_ADDR = readFile "devnet-env/utxo-keys/utxo1/utxo.addr"

env_TOKEN_NAME :: String
env_TOKEN_NAME = "TEST_TOKEN"

env_TOKEN_NAME_HEX :: IO String
env_TOKEN_NAME_HEX =
    Cmd.toChars [str|printf "%s" "#{env_TOKEN_NAME}"|]
        & Cmd.pipeChars "xxd -p"
        & Stream.takeWhile (/= '\n')
        & Stream.fold Fold.toList

env_POLICY_FILE :: FilePath
env_POLICY_FILE = "dev-local/policy.plutus"

env_VALIDATOR_FILE :: FilePath
env_VALIDATOR_FILE = "dev-local/validator.plutus"

env_WORK_DIR :: FilePath
env_WORK_DIR = "work"

_env_PROTO_PARAMS_FILE :: String
_env_PROTO_PARAMS_FILE = env_WORK_DIR </> "protocol.json"

env_TX_UNSIGNED :: String
env_TX_UNSIGNED = env_WORK_DIR </> "tx.unsigned"

env_TX_SIGNED :: String
env_TX_SIGNED = env_WORK_DIR </> "tx.signed"

-------------------------------------------------------------------------------
-- Command
-------------------------------------------------------------------------------

type Command = String
type CmdOption = (String, Maybe String)

opt :: (Show b) => String -> b -> CmdOption
opt a b = (a, Just (quoted b))
  where
    quoted = show

flg :: String -> CmdOption
flg a = (a, Nothing)

optNetwork :: CmdOption
optNetwork = opt "testnet-magic" env_CARDANO_TESTNET_MAGIC

optSocketPath :: CmdOption
optSocketPath = opt "socket-path" env_CARDANO_SOCKET

runCmd :: Command -> [CmdOption] -> Stream IO (Array Word8)
runCmd cmd args =
    Stream.before (putStrLn [str|> #{cmdStr}|]) (Cmd.toChunks cmdStr)
  where
    cmdList = cmd : concatMap (\(k, v) -> ["--" ++ k, maybe "" id v]) args
    cmdStr = unwords cmdList

getPolicyId :: FilePath -> IO String
getPolicyId scriptFile =
    runCmd
        "cardano-cli conway transaction policyid"
        [opt "script-file" scriptFile]
        & firstNonEmptyLine "getPolicyId"

getAddress :: FilePath -> IO String
getAddress scriptFile =
    runCmd
        "cardano-cli conway address build"
        [ optNetwork
        , opt "payment-script-file" scriptFile
        ]
        & firstNonEmptyLine "getAddress"

_saveProtoParams :: FilePath -> IO ()
_saveProtoParams outFile =
    runCmd
        "cardano-cli conway query protocol-parameters"
        [ optNetwork
        , optSocketPath
        , opt "out-file" outFile
        ]
        & drain

buildTransaction :: [CmdOption] -> IO ()
buildTransaction args =
    runCmd
        "cardano-cli conway transaction build"
        (optNetwork : optSocketPath : args)
        & drain

signTransaction :: [CmdOption] -> IO ()
signTransaction args =
    runCmd
        "cardano-cli conway transaction sign"
        (optNetwork : args)
        & drain

submitTransaction :: [CmdOption] -> IO ()
submitTransaction args =
    runCmd
        "cardano-cli conway transaction submit"
        (optNetwork : optSocketPath : args)
        & drain

getTransactionId :: String -> IO String
getTransactionId txSigned =
    runCmd
        "cardano-cli conway transaction txid"
        [ opt "tx-body-file" txSigned
        ]
        & Cmd.pipeChunks [str|jq -r ".txhash"|]
        & firstNonEmptyLine "getTransactionId"

getFirstUtxoAt :: String -> IO String
getFirstUtxoAt walletAddr =
    runCmd
        "cardano-cli conway query utxo"
        [ optNetwork
        , optSocketPath
        , opt "address" walletAddr
        ]
        & Cmd.pipeChunks [str|jq -r "keys[0]"|]
        & firstNonEmptyLine "getFirstUtxoAt"

-------------------------------------------------------------------------------
-- Output parsing
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

ensureBlankWorkDir :: IO ()
ensureBlankWorkDir = do
    Cmd.toStdout [str|rm -rf #{env_WORK_DIR}|]
    Cmd.toStdout [str|mkdir -p #{env_WORK_DIR}|]

main :: IO ()
main = do
    printStep "Setup"

    policyId <- getPolicyId env_POLICY_FILE
    faucetAddr <- env_FAUCET_WALLET_ADDR
    tokenNameHex <- env_TOKEN_NAME_HEX
    faucetUtxo <- getFirstUtxoAt faucetAddr
    validatorAddress <- getAddress env_VALIDATOR_FILE
    let assetClass = [str|#{policyId}.#{tokenNameHex}|]

    printVar "faucetAddr" faucetAddr
    printVar "validatorAddress" validatorAddress

    ensureBlankWorkDir
    printStep "Mint"
    buildTransaction
        [ opt "tx-in" faucetUtxo
        , opt "tx-in-collateral" faucetUtxo
        , opt "tx-out" [str|#{validatorAddress} + 2000000 + 100 #{assetClass}|]
        , opt "tx-out-inline-datum-value" (10 :: Int)
        , opt "tx-out-reference-script-file" env_VALIDATOR_FILE
        , opt "mint" [str|100 #{assetClass}|]
        , opt "mint-script-file" env_POLICY_FILE
        , opt "mint-redeemer-value" [str|{"constructor": 0, "fields": []}|]
        , opt "change-address" faucetAddr
        , opt "out-file" env_TX_UNSIGNED
        ]
    signTransaction
        [ opt "signing-key-file" env_FAUCET_WALLET_SKEY_FILE
        , opt "tx-body-file" env_TX_UNSIGNED
        , opt "out-file" env_TX_SIGNED
        ]
    submitTransaction
        [ opt "tx-file" env_TX_SIGNED
        ]

    mintTx <- getTransactionId env_TX_SIGNED
    printVar "mintTx" mintTx
    printStep "Waiting"
    threadDelay 5000000

    faucetUtxo1 <- getFirstUtxoAt faucetAddr
    printVar "faucetUtxo1" faucetUtxo1

    ensureBlankWorkDir
    printStep "Spend"
    let lockedUtxo = [str|#{mintTx}#0|]
    buildTransaction
        [ opt "tx-in" faucetUtxo1
        , opt "tx-in" lockedUtxo
        , opt "tx-in-collateral" faucetUtxo1
        , flg "spending-plutus-script-v2"
        , opt "spending-tx-in-reference" lockedUtxo
        , opt "spending-reference-tx-in-redeemer-value" (10 :: Int)
        , opt "tx-out" [str|#{validatorAddress} + 2000000 + 100 #{assetClass}|]
        , opt "tx-out-inline-datum-value" (20 :: Int)
        , opt "change-address" faucetAddr
        , opt "out-file" env_TX_UNSIGNED
        ]
    signTransaction
        [ opt "signing-key-file" env_FAUCET_WALLET_SKEY_FILE
        , opt "tx-body-file" env_TX_UNSIGNED
        , opt "out-file" env_TX_SIGNED
        ]
    submitTransaction
        [ opt "tx-file" env_TX_SIGNED
        ]
