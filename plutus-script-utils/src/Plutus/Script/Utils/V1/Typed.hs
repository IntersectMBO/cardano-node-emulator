{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutus.Script.Utils.V1.Typed
  ( ValidatorType,
    ConnectionError (..),
    WrongOutTypeError (..),
    checkValidatorAddress,
    checkRedeemer,
    checkDatum,
    TypedScriptTxOut (..),
    TypedScriptTxOutRef (..),
    ValidatorTypes (..),
    TypedValidator
      ( TypedValidator,
        tvValidator,
        tvValidatorHash,
        tvForwardingMintingPolicy,
        tvForwardingMintingPolicyHash,
        tvLanguage
      ),
    generalise,
    Any,
    IsDataDatum,
  )
where

import Control.Monad (unless)
import Control.Monad.Except (MonadError (throwError))
import Data.Aeson (FromJSON, ToJSON)
import Data.Coerce (coerce)
import Data.Kind (Type)
import Data.Void (Void)
import GHC.Generics (Generic)
import Plutus.Script.Utils.Scripts
  ( Language,
    MintingPolicy,
    MintingPolicyHash,
    Script,
    ToScript (toScript),
    ToValidator (toValidator),
    ToVersioned (toVersioned),
    Validator,
    ValidatorHash (ValidatorHash, getValidatorHash),
    Versioned (Versioned),
  )
import PlutusLedgerApi.V1
  ( Address (Address),
    BuiltinData,
    Credential (ScriptCredential),
    Datum (Datum),
    DatumHash,
    FromData (fromBuiltinData),
    Redeemer (Redeemer),
    ScriptContext,
    ScriptHash (ScriptHash),
    ToData,
    TxOut,
    TxOutRef,
    builtinDataToData,
  )
import Prettyprinter (Pretty (pretty), viaShow, (<+>))

data Any
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

-- | A class that associates a type standing for a connection type with two types, the type of the
-- redeemer and the data script for that connection type.
class ValidatorTypes (a :: Type) where
  -- | The type of the redeemers of this connection type.
  type RedeemerType a :: Type

  -- | Default redeemer type is ()
  type RedeemerType a = ()

  -- | The type of the data of this connection type.
  type DatumType a :: Type

  -- | Default datum type is ()
  type DatumType a = ()

instance ValidatorTypes ()

instance ValidatorTypes Void where
  type RedeemerType Void = Void
  type DatumType Void = Void

instance ValidatorTypes Any where
  type RedeemerType Any = BuiltinData
  type DatumType Any = BuiltinData

-- | A validator, its hash, its forwarding minting policy and hash bundled into
-- a structure tagged by a phantom connection type.
data TypedValidator (a :: Type) = TypedValidator
  { -- | The validator script comprised in the typed validator
    tvValidator :: Validator,
    -- | The hash of the validator script
    tvValidatorHash :: ValidatorHash,
    -- | The forwarding minting policy associated with the validator hash, which
    -- ensures the validator is invoked in a transaction.
    tvForwardingMintingPolicy :: MintingPolicy,
    -- | The hash of the forwarding minting policy associated to this validator
    tvForwardingMintingPolicyHash :: MintingPolicyHash,
    -- | The language of the validator and forwarding minting policy
    tvLanguage :: Language
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance ToScript (TypedValidator a) where
  toScript = toScript . tvValidator

instance ToValidator (TypedValidator a) where
  toValidator = toValidator . tvValidator

instance ToVersioned Script (TypedValidator a) where
  toVersioned = (toScript <$>) . toVersioned @Validator

instance ToVersioned Validator (TypedValidator a) where
  toVersioned (TypedValidator {tvLanguage, tvValidator}) = Versioned tvValidator tvLanguage

-- | Generalise the typed validator to one that works with the 'Data' type.  we
-- can do this safely because the on-chain validators are untyped, so they
-- always take 'BuiltinData' arguments. The validator script stays the same, so
-- the conversion from 'BuiltinData' to 'a' still takes place, even if it's not
-- reflected in the type signature anymore.
generalise :: TypedValidator a -> TypedValidator Any
generalise = coerce

-- | The type of validators for the given connection type.
type ValidatorType (a :: Type) = DatumType a -> RedeemerType a -> ScriptContext -> Bool

data WrongOutTypeError
  = ExpectedScriptGotPubkey
  | ExpectedPubkeyGotScript
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | An error we can get while trying to type an existing transaction part.
data ConnectionError
  = WrongValidatorHash ValidatorHash ValidatorHash
  | WrongCredentialType WrongOutTypeError
  | WrongValidatorType String
  | WrongRedeemerType BuiltinData
  | WrongDatumType BuiltinData
  | NoDatum TxOutRef DatumHash
  | UnknownRef TxOutRef
  deriving stock (Show, Eq, Ord, Generic)

instance Pretty ConnectionError where
  pretty (WrongValidatorHash h1 h2) = "Wrong validator address. Expected:" <+> pretty h1 <+> "Actual:" <+> pretty h2
  pretty (WrongCredentialType t) = "Wrong credential type:" <+> viaShow t
  pretty (WrongValidatorType t) = "Wrong validator type:" <+> pretty t
  pretty (WrongRedeemerType d) = "Wrong redeemer type" <+> pretty (builtinDataToData d)
  pretty (WrongDatumType d) = "Wrong datum type" <+> pretty (builtinDataToData d)
  pretty (NoDatum t d) = "No datum with hash " <+> pretty d <+> "for tx output" <+> pretty t
  pretty (UnknownRef d) = "Unknown reference" <+> pretty d

-- | Checks that the given address is consistent with the actual validator.
checkValidatorAddress ::
  (MonadError ConnectionError m) => TypedValidator a -> Address -> m ()
checkValidatorAddress (TypedValidator {tvValidatorHash}) (Address (ScriptCredential (ScriptHash sHash)) _) =
  unless (sHash == getValidatorHash tvValidatorHash) $
    throwError $
      WrongValidatorHash tvValidatorHash (ValidatorHash sHash)
checkValidatorAddress _ _ = throwError $ WrongCredentialType ExpectedScriptGotPubkey

-- | Checks that the given redeemer script has the right type.
checkRedeemer ::
  (FromData (RedeemerType inn), MonadError ConnectionError m) =>
  TypedValidator inn ->
  Redeemer ->
  m (RedeemerType inn)
checkRedeemer _ (Redeemer d) =
  case fromBuiltinData d of
    Just v -> pure v
    Nothing -> throwError $ WrongRedeemerType d

-- | Checks that the given datum has the right type.
checkDatum ::
  (FromData (DatumType a), MonadError ConnectionError m) =>
  TypedValidator a ->
  Datum ->
  m (DatumType a)
checkDatum _ (Datum d) =
  case fromBuiltinData d of
    Just v -> pure v
    Nothing -> throwError $ WrongDatumType d

type IsDataDatum a = (FromData (DatumType a), ToData (DatumType a))

-- | A 'TxOut' tagged by a phantom type: the connection type of the output.
data TypedScriptTxOut a
  = (IsDataDatum a) =>
  TypedScriptTxOut
  { tyTxOutTxOut :: TxOut,
    tyTxOutData :: DatumType a
  }

deriving stock instance (Eq (DatumType a)) => Eq (TypedScriptTxOut a)

-- | A 'TxOutRef' tagged by a phantom type: the connection type of the output.
data TypedScriptTxOutRef a = TypedScriptTxOutRef
  { tyTxOutRefRef :: TxOutRef,
    tyTxOutRefOut :: TypedScriptTxOut a
  }

deriving stock instance (Eq (DatumType a)) => Eq (TypedScriptTxOutRef a)
