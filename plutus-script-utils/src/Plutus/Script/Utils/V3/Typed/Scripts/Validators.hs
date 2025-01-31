{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
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
  WrongOutTypeError (..),
  ConnectionError (..),
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
  checkValidatorAddress,
  checkRedeemer,
  checkDatum,
)
where

import Control.Monad (unless)
import Control.Monad.Except (MonadError (throwError))
import Data.Aeson (FromJSON, ToJSON)
import Data.Kind (Type)
import GHC.Generics (Generic)
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
import Prettyprinter (Pretty (pretty), viaShow, (<+>))

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

data WrongOutTypeError
  = ExpectedScriptGotPubkey
  | ExpectedPubkeyGotScript
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | An error we can get while trying to type an existing transaction part.
data ConnectionError
  = WrongValidatorAddress PV3.Address PV3.Address
  | WrongOutType WrongOutTypeError
  | WrongValidatorType String
  | WrongRedeemerType PV3.BuiltinData
  | WrongDatumType PV3.BuiltinData
  | NoDatum PV3.TxOutRef PV3.DatumHash
  | UnknownRef PV3.TxOutRef
  deriving stock (Show, Eq, Ord, Generic)

instance Pretty ConnectionError where
  pretty (WrongValidatorAddress a1 a2) = "Wrong validator address. Expected:" <+> pretty a1 <+> "Actual:" <+> pretty a2
  pretty (WrongOutType t) = "Wrong out type:" <+> viaShow t
  pretty (WrongValidatorType t) = "Wrong validator type:" <+> pretty t
  pretty (WrongRedeemerType d) = "Wrong redeemer type" <+> pretty (PV3.builtinDataToData d)
  pretty (WrongDatumType d) = "Wrong datum type" <+> pretty (PV3.builtinDataToData d)
  pretty (NoDatum t d) = "No datum with hash " <+> pretty d <+> "for tx output" <+> pretty t
  pretty (UnknownRef d) = "Unknown reference" <+> pretty d

-- | Checks that the given validator hash is consistent with the actual validator.
checkValidatorAddress
  :: forall a m. (MonadError ConnectionError m) => TypedValidator a -> PV3.Address -> m ()
checkValidatorAddress ct actualAddr = do
  let expectedAddr = validatorAddress ct
  unless (expectedAddr == actualAddr) $ throwError $ WrongValidatorAddress expectedAddr actualAddr

-- | Checks that the given redeemer script has the right type.
checkRedeemer
  :: forall inn m
   . (PV3.FromData (RedeemerType inn), MonadError ConnectionError m)
  => TypedValidator inn
  -> PV3.Redeemer
  -> m (RedeemerType inn)
checkRedeemer _ (PV3.Redeemer d) =
  case PV3.fromBuiltinData d of
    Just v -> pure v
    Nothing -> throwError $ WrongRedeemerType d

-- | Checks that the given datum has the right type.
checkDatum
  :: forall a m
   . (PV3.FromData (DatumType a), MonadError ConnectionError m)
  => TypedValidator a
  -> PV3.Datum
  -> m (DatumType a)
checkDatum _ (PV3.Datum d) =
  case PV3.fromBuiltinData d of
    Just v -> pure v
    Nothing -> throwError $ WrongDatumType d
