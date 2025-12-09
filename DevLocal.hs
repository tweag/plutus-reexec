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

import Control.Monad (void)
import Streamly.System.Command qualified as Cmd
import Streamly.Unicode.String (str)
import System.FilePath ((<.>), (</>))

-------------------------------------------------------------------------------
-- Types & Utils
-------------------------------------------------------------------------------

-- Minimal environment (set these globals when constructing Env)
data Env = Env
    { cliPath :: String -- e.g. "cardano-cli"
    , workDir :: FilePath -- e.g. "work"
    , networkArg :: String -- e.g. "--testnet-magic 42"
    , faucetUtxo :: String -- e.g. "txhash#0"
    , faucetSkey :: FilePath
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

createWallet :: Env -> String -> IO ()
createWallet Env{..} name = do
    printStep "createWallet"
    cmd_ [str|mkdir -p #{workDir}|]
    let vkey = workDir </> name <.> "vkey"
        skey = workDir </> name <.> "skey"
        addr = workDir </> name <.> "addr"
    cmd_
        [str|
    #{cliPath} address key-gen
             --verification-key-file #{vkey}
             --signing-key-file #{skey}
        |]

    cmd_
        [str|
    #{cliPath} address build
             --payment-verification-key-file #{vkey} #{networkArg}
             --out-file #{addr}
        |]

    putStrLn $ "[createWallet] wrote: " ++ vkey ++ " , " ++ skey ++ " , " ++ addr

fundWallet :: Env -> String -> Integer -> IO ()
fundWallet Env{..} destAddr lovelace = do
    printStep "fundWallet"
    let raw = workDir </> "fund.raw"
        signed = workDir </> "fund.signed"
        lovelaceStr = show lovelace
    cmd_
        [str|
    #{cliPath} transaction build
             --babbage-era
             --tx-in #{faucetUtxo}
             --tx-out #{destAddr}+#{lovelaceStr}
             --change-address #{destAddr}
             #{networkArg}
             --out-file #{raw}
        |]
    cmd_
        [str|
    #{cliPath} transaction sign
             --tx-body-file #{raw}
             --signing-key-file #{faucetSkey}
             #{networkArg}
             --out-file #{signed}
        |]
    cmd_
        [str|
    #{cliPath} transaction submit
             --tx-file #{signed}
             #{networkArg}
        |]

writeProtocolParams :: Env -> IO FilePath
writeProtocolParams Env{..} = do
    printStep "writeProtocolParams"
    let out = workDir </> "protocol.json"
    cmd_
        [str|
    #{cliPath} query protocol-parameters #{networkArg} --out-file #{out}
        |]
    pure out

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = do
    let env =
            Env
                { cliPath = "cardano-cli"
                , workDir = "work"
                , networkArg = "--testnet-magic 42"
                , faucetUtxo = "REPLACE_TXHASH#0"
                , faucetSkey = "faucet.skey"
                }

    -- create wallets
    createWallet env "alice"
    createWallet env "bob"

    -- fund alice
    aliceAddr <- readFile "work/alice.addr"
    fundWallet env aliceAddr 5000000

    -- protocol params for minting
    void $ writeProtocolParams env

    pure ()
