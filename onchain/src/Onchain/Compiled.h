#ifdef REMOVE_TRACE
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:remove-trace #-}
#endif

module MODULE_NAME where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Onchain.Simple
import PlutusTx
import PlutusTx.Prelude
import Cardano.Api

--------------------------------------------------------------------------------
-- Compiled
--------------------------------------------------------------------------------

alwaysTrue :: CompiledCodeLang PlutusScriptV3 (BuiltinData -> BuiltinUnit); \
alwaysTrue = CompiledCodeLang $$(PlutusTx.compile [||alwaysTrue'||])
