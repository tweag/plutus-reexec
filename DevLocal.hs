{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

-- TODO: Use cardano-cli as a haskell library

module Main (
    main,
) where

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

import Data.Function ((&))
import Streamly.Data.Fold qualified as Fold
import Streamly.Data.Stream qualified as Stream
import Streamly.System.Command qualified as Cmd
import Streamly.Unicode.Stream qualified as Unicode
import Streamly.Unicode.String (str)
import System.Directory (doesFileExist)
import System.Environment (getArgs, setEnv)
import System.FilePath ((<.>), (</>))

-------------------------------------------------------------------------------
-- Types & Utils
-------------------------------------------------------------------------------

data CommonEnv = CommonEnv
    { cCardanoCli :: String
    , cCardanoNode :: FilePath
    , cCardanoTestnet :: String
    , cNetworkArg :: String
    }

data PopulateEnv = PopulateEnv
    { pCommon :: CommonEnv
    , pSocketPath :: FilePath
    , pWorkDir :: FilePath -- "work"
    , pFaucetUtxoDir :: String
    }

echoing :: (String -> IO a) -> String -> IO a
echoing act cmdStr = do
    putStrLn cmdStr
    act cmdStr

-- NOTE: This may potentially change the command. We remove the space for
-- prettification.
oneLine :: String -> String
oneLine = unwords . map (dropWhile (== ' ')) . filter (not . null) . lines

firstLine :: String -> String
firstLine = takeWhile (not . (== '\n'))

cmd_ :: String -> IO ()
cmd_ = echoing Cmd.toStdout . oneLine

cmd :: String -> IO String
cmd = echoing Cmd.toString . oneLine

printStep :: String -> IO ()
printStep s = putStrLn . unlines $ ["", divider, s, divider]
  where
    divider = replicate 80 '-'

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

createWallet :: PopulateEnv -> String -> IO ()
createWallet PopulateEnv{..} name = do
    let CommonEnv{..} = pCommon
    printStep "createWallet"
    cmd_ [str|mkdir -p #{pWorkDir}|]
    let vkey = pWorkDir </> name <.> "vkey"
        skey = pWorkDir </> name <.> "skey"
        addr = pWorkDir </> name <.> "addr"
    addrExists <- doesFileExist addr
    if addrExists
        then do
            putStrLn [str|#{name} exists. Skipping...|]
        else do
            cmd_
                [str|
    #{cCardanoCli} address key-gen
             --verification-key-file #{vkey}
             --signing-key-file #{skey}
        |]

            cmd_
                [str|
    #{cCardanoCli} address build
             --payment-verification-key-file #{vkey} #{cNetworkArg}
             --out-file #{addr}
        |]

            putStrLn $ "[createWallet] wrote: " ++ vkey ++ " , " ++ skey ++ " , " ++ addr

queryFirstUtxo :: PopulateEnv -> String -> IO String
queryFirstUtxo PopulateEnv{..} addr = do
    let CommonEnv{..} = pCommon
    let queryUtxoCmd =
            oneLine $
                [str|
             cardano-cli query utxo
                 #{cNetworkArg}
                 --socket-path #{pSocketPath}
                 --address #{addr}
                 |]
    Cmd.toChunks queryUtxoCmd
        & Cmd.pipeChunks [str|jq -r 'keys[0]'|]
        & Unicode.decodeUtf8Chunks
        & Stream.fold (Fold.toList)

fundWallet :: PopulateEnv -> String -> Integer -> IO ()
fundWallet penv@PopulateEnv{..} destAddr lovelace = do
    let CommonEnv{..} = pCommon
    printStep "fundWallet"
    let raw = pWorkDir </> "fund.raw"
        signed = pWorkDir </> "fund.signed"
        lovelaceStr = show lovelace
    faucetAddr <- readFile (pFaucetUtxoDir </> "utxo.addr")
    faucetUtxo <- queryFirstUtxo penv faucetAddr
    cmd_
        [str|
    #{cCardanoCli} latest transaction build
             #{cNetworkArg}
             --socket-path #{pSocketPath}
             --tx-in #{faucetUtxo}
             --change-address #{faucetAddr}
             --tx-out #{destAddr}+#{lovelaceStr}
             --out-file #{raw}
        |]
    let faucetUtxoKeyFile = pFaucetUtxoDir </> "utxo.skey"
    cmd_
        [str|
    #{cCardanoCli} latest transaction sign
             #{cNetworkArg}
             --tx-body-file #{raw}
             --signing-key-file #{faucetUtxoKeyFile}
             --out-file #{signed}
        |]
    cmd_
        [str|
    #{cCardanoCli} latest transaction submit
             #{cNetworkArg}
             --socket-path #{pSocketPath}
             --tx-file #{signed}
        |]

writeProtocolParams :: PopulateEnv -> IO FilePath
writeProtocolParams PopulateEnv{..} = do
    let CommonEnv{..} = pCommon
    printStep "writeProtocolParams"
    let out = pWorkDir </> "protocol.json"
    cmd_
        [str|
    #{cCardanoCli} query protocol-parameters
             #{cNetworkArg}
             --socket-path #{pSocketPath}
             --out-file #{out}
        |]
    pure out

--------------------------------------------------------------------------------
-- Populate
--------------------------------------------------------------------------------

populate :: PopulateEnv -> IO ()
populate env = do
    -- create wallets
    createWallet env "alice"
    createWallet env "bob"

    -- fund alice
    aliceAddr <- readFile "work/alice.addr"
    fundWallet env aliceAddr 5000000

    -- protocol params for minting
    protoParams <- writeProtocolParams env

    putStrLn protoParams

--------------------------------------------------------------------------------
-- DevnetEnv
--------------------------------------------------------------------------------

data DevnetEnv = DevnetEnv
    { dCommon :: CommonEnv
    , dNumPoolNodes :: Int -- 1
    , dOutputDir :: FilePath -- "./devnet-env"
    }

runDevnet :: DevnetEnv -> IO ()
runDevnet DevnetEnv{..} = do
    let CommonEnv{..} = dCommon
    cmd_ [str|mkdir -p #{dOutputDir}|]
    let dNumPoolNodesStr = show dNumPoolNodes
    setEnv "CARDANO_CLI" cCardanoCli
    setEnv "CARDANO_NODE" cCardanoNode
    cmd_
        [str|
    #{cCardanoTestnet} cardano
              --num-pool-nodes #{dNumPoolNodesStr}
              --conway-era
              --enable-new-epoch-state-logging
              --output-dir #{dOutputDir}
              #{cNetworkArg}
        |]

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["start-local-net"] -> runDevnet devnetEnv
        ["populate-local-net"] -> populate populateEnv
        _ -> putStrLn "Unknown behavior."
  where
    commonEnv =
        CommonEnv
            { cCardanoCli = "cardano-cli"
            , cCardanoNode = "cardano-node"
            , cCardanoTestnet = "cardano-testnet"
            , cNetworkArg = "--testnet-magic 42"
            }

    devnetEnv =
        DevnetEnv
            { dCommon = commonEnv
            , dNumPoolNodes = 1
            , dOutputDir = "./devnet-env"
            }

    populateEnv =
        PopulateEnv
            { pCommon = commonEnv
            , pSocketPath = "./devnet-env/socket/node1/sock"
            , pWorkDir = "work"
            , pFaucetUtxoDir = "devnet-env/utxo-keys/utxo1"
            }
