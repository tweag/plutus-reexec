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

import Streamly.System.Command qualified as Cmd
import Streamly.Unicode.String (str)
import System.Environment (getArgs)
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
    , pWorkDir :: FilePath -- "work"
    , pFaucetUtxo :: String -- "txhash#0"
    , pFaucetSkey :: FilePath
    }

echoing :: (String -> IO a) -> String -> IO a
echoing act cmdStr = do
    putStrLn cmdStr
    act cmdStr

oneLine :: String -> String
oneLine = unwords . lines

cmd_ :: String -> IO ()
cmd_ = echoing Cmd.toStdout . oneLine

cmd :: String -> IO String
cmd = echoing Cmd.toString . oneLine

printStep :: String -> IO ()
printStep s = putStrLn . unlines $ [divider, s, divider]
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

fundWallet :: PopulateEnv -> String -> Integer -> IO ()
fundWallet PopulateEnv{..} destAddr lovelace = do
    let CommonEnv{..} = pCommon
    printStep "fundWallet"
    let raw = pWorkDir </> "fund.raw"
        signed = pWorkDir </> "fund.signed"
        lovelaceStr = show lovelace
    cmd_
        [str|
    #{cCardanoCli} transaction build
             --babbage-era
             --tx-in #{pFaucetUtxo}
             --tx-out #{destAddr}+#{lovelaceStr}
             --change-address #{destAddr}
             #{cNetworkArg}
             --out-file #{raw}
        |]
    cmd_
        [str|
    #{cCardanoCli} transaction sign
             --tx-body-file #{raw}
             --signing-key-file #{pFaucetSkey}
             #{cNetworkArg}
             --out-file #{signed}
        |]
    cmd_
        [str|
    #{cCardanoCli} transaction submit
             --tx-file #{signed}
             #{cNetworkArg}
        |]

writeProtocolParams :: PopulateEnv -> IO FilePath
writeProtocolParams PopulateEnv{..} = do
    let CommonEnv{..} = pCommon
    printStep "writeProtocolParams"
    let out = pWorkDir </> "protocol.json"
    cmd_
        [str|
    #{cCardanoCli} query protocol-parameters #{cNetworkArg} --out-file #{out}
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
    cmd_
        [str|
    CARDANO_CLI=#{cCardanoCli}
    CARDANO_NODE=#{cCardanoNode}
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
            , pWorkDir = "work"
            , pFaucetUtxo = "REPLACE_TXHASH#0"
            , pFaucetSkey = "faucet.skey"
            }
