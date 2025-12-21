{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Cardano.Api (
    File (..),
    IsPlutusScriptLanguage,
    PlutusScript,
    PlutusScriptV2,
    PlutusScriptVersion (PlutusScriptV2),
    writeFileTextEnvelope,
 )

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Cardano.Api.Plutus (PlutusScript (..))
import Data.ByteString.Short qualified as SBS
import Data.Proxy (Proxy)
import Onchain.Debug qualified as Debug
import Onchain.Release qualified as Release
import Onchain.Simple (CompiledCodeLang (..))
import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>))

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

writePlutusScript ::
    forall lang a.
    (IsPlutusScriptLanguage lang) =>
    FilePath -> CompiledCodeLang lang a -> IO ()
writePlutusScript filename (CompiledCodeLang compiledCode) = do
    let serialisedScript = serialiseCompiledCode compiledCode
        script = PlutusScriptSerialised serialisedScript :: PlutusScript lang
    result <- writeFileTextEnvelope (File filename) Nothing script
    case result of
        Left err -> print err
        Right () -> putStrLn $ "Successfully wrote script to: " ++ filename

printHelpText :: IO ()
printHelpText = putStrLn "Usage: <exe> --local-config-dir PATH"

main :: IO ()
main = do
    argList <- getArgs
    case argList of
        ["--local-config-dir", x] -> do
            writePlutusScript (x </> "policy.plutus") Release.alwaysTrue
            writePlutusScript (x </> "validator.plutus") Release.alwaysTrue
            writePlutusScript (x </> "policy-debug.plutus") Debug.alwaysTrue
            writePlutusScript (x </> "validator-debug.plutus") Debug.alwaysTrue
        _ -> printHelpText >> exitFailure
