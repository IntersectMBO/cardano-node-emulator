{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}

module Plutus.Script.Utils.V3.Typed.Scripts.Validators (
  UntypedValidator,
  ---
  ValidatorTypes (..),
  ValidatorType,
  TypedValidator,
  mkTypedValidator,
  mkTypedValidatorParam,
  validatorHash,
  validatorAddress,
  validatorScript,
  vValidatorScript,
  forwardingMintingPolicy,
  vForwardingMintingPolicy,
  forwardingMintingPolicyHash,
  generalise,
)
where

import Data.Kind (Type)
import Plutus.Script.Utils.Scripts (Language (PlutusV3), Versioned (Versioned), mkValidatorScript)
import Plutus.Script.Utils.Typed (
  DatumType,
  RedeemerType,
  TypedValidator (TypedValidator, tvForwardingMPS, tvForwardingMPSHash, tvValidator, tvValidatorHash),
  UntypedValidator,
  ValidatorTypes,
  forwardingMintingPolicy,
  forwardingMintingPolicyHash,
  generalise,
  vForwardingMintingPolicy,
  vValidatorScript,
  validatorAddress,
  validatorHash,
  validatorScript,
 )
import Plutus.Script.Utils.V3.Scripts qualified as Scripts
import Plutus.Script.Utils.V3.Typed.Scripts.MonetaryPolicies qualified as MPS
import PlutusCore.Default (DefaultUni)
import PlutusCore.Version (plcVersion110)
import PlutusLedgerApi.V3 qualified as PV3
import PlutusTx (CompiledCode, Lift, liftCode, unsafeApplyCode)

-- | The type of validators for the given connection type.
type ValidatorType (a :: Type) = DatumType a -> RedeemerType a -> PV3.ScriptContext -> Bool

-- | Make a 'TypedValidator' from the 'CompiledCode' of a validator script and its wrapper.
mkTypedValidator
  :: CompiledCode (ValidatorType a)
  -- ^ Validator script (compiled)
  -> CompiledCode (ValidatorType a -> UntypedValidator)
  -- ^ A wrapper for the compiled validator
  -> TypedValidator a
mkTypedValidator vc wrapper =
  TypedValidator
    { tvValidator = Versioned val PlutusV3
    , tvValidatorHash = hsh
    , tvForwardingMPS = Versioned mps PlutusV3
    , tvForwardingMPSHash = Scripts.mintingPolicyHash mps
    }
  where
    val = mkValidatorScript $ wrapper `unsafeApplyCode` vc
    hsh = Scripts.validatorHash val
    mps = MPS.mkForwardingMintingPolicy hsh

-- | Make a 'TypedValidator' from the 'CompiledCode' of a parameterized validator script and its wrapper.
mkTypedValidatorParam
  :: forall a param
   . (Lift DefaultUni param)
  => CompiledCode (param -> ValidatorType a)
  -- ^ Validator script (compiled)
  -> CompiledCode (ValidatorType a -> UntypedValidator)
  -- ^ A wrapper for the compiled validator
  -> param
  -- ^ The extra paramater for the validator script
  -> TypedValidator a
mkTypedValidatorParam vc wrapper param =
  mkTypedValidator (vc `unsafeApplyCode` liftCode plcVersion110 param) wrapper
