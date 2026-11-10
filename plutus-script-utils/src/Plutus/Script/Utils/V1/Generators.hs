{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -g -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

module Plutus.Script.Utils.V1.Generators
  ( alwaysSucceedValidator,
    alwaysFailValidator,
    alwaysSucceedValidatorVersioned,
    alwaysFailValidatorVersioned,
    alwaysSucceedPolicy,
    alwaysFailPolicy,
    alwaysSucceedPolicyVersioned,
    alwaysFailPolicyVersioned,
    mkForwardingMintingPolicy,
    mkForwardingStakeValidator,
  )
where

import Plutus.Script.Utils.Scripts
  ( Language (PlutusV1),
    MintingPolicy,
    StakeValidator,
    Validator,
    ValidatorHash (ValidatorHash),
    Versioned (Versioned),
    toMintingPolicy,
    toStakeValidator,
    toValidator,
  )
import Plutus.Script.Utils.V1.Scripts
  ( mkUntypedMintingPolicy,
    mkUntypedStakeValidator,
  )
import PlutusCore.Version (plcVersion100)
import PlutusLedgerApi.V1
  ( Address (Address),
    Credential (ScriptCredential),
    ScriptContext (ScriptContext),
    ScriptHash (ScriptHash),
    ScriptPurpose (Certifying, Minting, Rewarding),
    TxInInfo (txInInfoResolved),
    TxInfo (TxInfo, txInfoInputs),
    TxOut (TxOut),
  )
import PlutusTx qualified
import PlutusTx.Builtins.Internal qualified as PlutusTx
import PlutusTx.List (any)
import PlutusTx.Prelude (Bool (False), ($), (.), (==))

alwaysSucceedValidator :: Validator
alwaysSucceedValidator = toValidator $$(PlutusTx.compile [||trueVal||])
  where
    trueVal ::
      PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinUnit
    trueVal _ _ _ = PlutusTx.unitval

alwaysFailValidator :: Validator
alwaysFailValidator = toValidator $$(PlutusTx.compile [||falseVal||])
  where
    falseVal ::
      PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinUnit
    falseVal _ _ _ = PlutusTx.error PlutusTx.unitval

alwaysSucceedValidatorVersioned :: Versioned Validator
alwaysSucceedValidatorVersioned = Versioned alwaysSucceedValidator PlutusV1

alwaysFailValidatorVersioned :: Versioned Validator
alwaysFailValidatorVersioned = Versioned alwaysFailValidator PlutusV1

alwaysSucceedPolicy :: MintingPolicy
alwaysSucceedPolicy = toMintingPolicy $$(PlutusTx.compile [||trueMP||])
  where
    trueMP :: PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinUnit
    trueMP _ _ = PlutusTx.unitval

alwaysFailPolicy :: MintingPolicy
alwaysFailPolicy = toMintingPolicy $$(PlutusTx.compile [||falseMP||])
  where
    falseMP :: PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinUnit
    falseMP _ _ = PlutusTx.error PlutusTx.unitval

alwaysSucceedPolicyVersioned :: Versioned MintingPolicy
alwaysSucceedPolicyVersioned = Versioned alwaysSucceedPolicy PlutusV1

alwaysFailPolicyVersioned :: Versioned MintingPolicy
alwaysFailPolicyVersioned = Versioned alwaysFailPolicy PlutusV1

-- | A minting policy that checks whether the validator script was run
-- in the minting transaction.
mkForwardingMintingPolicy :: ValidatorHash -> MintingPolicy
mkForwardingMintingPolicy vshsh =
  toMintingPolicy
    $ $$(PlutusTx.compile [||mkUntypedMintingPolicy . forwardToValidator||])
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 vshsh
  where
    {-# INLINEABLE forwardToValidator #-}
    forwardToValidator :: ValidatorHash -> () -> ScriptContext -> Bool
    forwardToValidator (ValidatorHash h) _ (ScriptContext (TxInfo {txInfoInputs}) (Minting _)) =
      let checkHash (TxOut (Address (ScriptCredential (ScriptHash vh)) _) _ _) = vh == h
          checkHash _ = False
       in any (checkHash . txInInfoResolved) txInfoInputs
    forwardToValidator _ _ _ = False

-- | A stake validator that checks whether the validator script was run
-- in the right transaction.
mkForwardingStakeValidator :: ValidatorHash -> StakeValidator
mkForwardingStakeValidator vshsh =
  toStakeValidator
    $ $$(PlutusTx.compile [||mkUntypedStakeValidator . forwardToValidator||])
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 vshsh
  where
    {-# INLINEABLE forwardToValidator #-}
    forwardToValidator :: ValidatorHash -> () -> ScriptContext -> Bool
    forwardToValidator (ValidatorHash h) _ (ScriptContext (TxInfo {txInfoInputs}) scriptContextPurpose) =
      let checkHash (TxOut (Address (ScriptCredential (ScriptHash vh)) _) _ _) = vh == h
          checkHash _ = False
          result = any (checkHash . txInInfoResolved) txInfoInputs
       in case scriptContextPurpose of
            Rewarding _ -> result
            Certifying _ -> result
            _ -> False
