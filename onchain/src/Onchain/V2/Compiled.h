#ifdef REMOVE_TRACE
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:remove-trace #-}
#endif
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

module MODULE_NAME where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Onchain.V2.Simple (CompiledCodeLang (..))
import Onchain.V2.Simple qualified as Simple
import Onchain.V2.Escrow qualified as Escrow
import PlutusTx
import PlutusTx.Prelude
import Cardano.Api
import PlutusCore.Version (plcVersion100)

--------------------------------------------------------------------------------
-- Compiled
--------------------------------------------------------------------------------

type Policy = BuiltinData -> BuiltinData -> BuiltinUnit
type Validator = BuiltinData -> BuiltinData -> BuiltinData -> BuiltinUnit
type CompiledCodePolicy = CompiledCodeLang PlutusScriptV2 Policy
type CompiledCodeValidator = CompiledCodeLang PlutusScriptV2 Validator

tracingPolicy :: CompiledCodePolicy
tracingPolicy = CompiledCodeLang $$(PlutusTx.compile [|| Simple.policy ||])

tracingValidator :: CompiledCodeValidator
tracingValidator = CompiledCodeLang $$(PlutusTx.compile [|| Simple.validator ||])

escrowValidator :: Escrow.EscrowParams -> CompiledCodeValidator
escrowValidator params =
    CompiledCodeLang
        ($$(PlutusTx.compile [|| Escrow.validatorUntyped ||]) `unsafeApplyCode` liftCode plcVersion100 params)
