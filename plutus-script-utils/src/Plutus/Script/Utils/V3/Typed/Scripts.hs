{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Plutus.Script.Utils.V3.Typed.Scripts
  ( module Plutus.Script.Utils.V3.Typed.Scripts.MultiPurpose,
    Validator,
    MintingPolicy,
    StakeValidator,
    TypedScriptTxOut (..),
    TypedScriptTxOutRef (..),
    typeScriptTxOut,
    typeScriptTxOutRef,
    ConnectionError (..),
  )
where

import Control.Monad (unless)
import Control.Monad.Except (MonadError (throwError))
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Plutus.Script.Utils.Scripts (MintingPolicy, StakeValidator, Validator, datumHash)
import Plutus.Script.Utils.V3.Typed.Scripts.MultiPurpose
import PlutusLedgerApi.V1 qualified as PV1
import PlutusLedgerApi.V3
  ( BuiltinData,
    Credential (PubKeyCredential, ScriptCredential),
    Datum,
    DatumHash,
    FromData,
    OutputDatum (OutputDatum, OutputDatumHash),
    ToData (..),
    TxOut (txOutAddress, txOutDatum),
    TxOutRef,
    addressCredential,
  )
import Prettyprinter (Pretty (pretty), viaShow, (<+>))

data WrongOutTypeError
  = ExpectedScriptGotPubkey
  | ExpectedPubkeyGotScript
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | An error we can get while trying to type an existing transaction part.
data ConnectionError
  = WrongValidatorAddress PV1.Address PV1.Address
  | WrongOutType WrongOutTypeError
  | WrongValidatorType String
  | WrongRedeemerType BuiltinData
  | WrongDatumType BuiltinData
  | NoDatum TxOutRef DatumHash
  | UnknownRef TxOutRef
  deriving stock (Show, Eq, Ord, Generic)

instance Pretty ConnectionError where
  pretty (WrongValidatorAddress a1 a2) = "Wrong validator address. Expected:" <+> pretty a1 <+> "Actual:" <+> pretty a2
  pretty (WrongOutType t) = "Wrong out type:" <+> viaShow t
  pretty (WrongValidatorType t) = "Wrong validator type:" <+> pretty t
  pretty (WrongRedeemerType d) = "Wrong redeemer type" <+> pretty (PV1.builtinDataToData d)
  pretty (WrongDatumType d) = "Wrong datum type" <+> pretty (PV1.builtinDataToData d)
  pretty (NoDatum t d) = "No datum with hash " <+> pretty d <+> "for tx output" <+> pretty t
  pretty (UnknownRef d) = "Unknown reference" <+> pretty d

-- | Checks that the given validator hash is consistent with the actual validator.
checkValidatorAddress ::
  forall a m. (MonadError ConnectionError m) => MultiPurposeScript a -> PV1.Address -> m ()
checkValidatorAddress ct actualAddr = do
  let expectedAddr = multiPurposeScriptAddress ct
  unless (expectedAddr == actualAddr) $ throwError $ WrongValidatorAddress expectedAddr actualAddr

-- -- | Checks that the given redeemer script has the right type.
-- checkRedeemerForPurpose ::
--   forall inn m.
--   (PV1.FromData (RedeemerType inn), MonadError ConnectionError m) =>
--   TypedValidator inn ->
--   PV1.Redeemer ->
--   m (RedeemerType inn)
-- checkRedeemerForPurpose _ (PV1.Redeemer d) =
--   case PV1.fromBuiltinData d of
--     Just v -> pure v
--     Nothing -> throwError $ WrongRedeemerType d

-- | Checks that the given datum has the right type.
checkDatum ::
  forall a m.
  (PV1.FromData (DatumType a), MonadError ConnectionError m) =>
  MultiPurposeScript a ->
  Datum ->
  m (DatumType a)
checkDatum _ (PV1.Datum d) =
  case PV1.fromBuiltinData @(DatumType a) d of
    Just v -> pure v
    Nothing -> throwError $ WrongDatumType d

-- | A 'TxOut' tagged by a phantom type: and the connection type of the output.
data TypedScriptTxOut a = (FromData (DatumType a), ToData (DatumType a)) =>
  TypedScriptTxOut
  { tyTxOutTxOut :: TxOut,
    tyTxOutData :: DatumType a
  }

instance (Eq (DatumType a)) => Eq (TypedScriptTxOut a) where
  l == r =
    tyTxOutTxOut l == tyTxOutTxOut r
      && tyTxOutData l == tyTxOutData r

-- | A 'TxOutRef' tagged by a phantom type: and the connection type of the output.
data TypedScriptTxOutRef a = TypedScriptTxOutRef
  { tyTxOutRefRef :: TxOutRef,
    tyTxOutRefOut :: TypedScriptTxOut a
  }

instance (Eq (DatumType a)) => Eq (TypedScriptTxOutRef a) where
  l == r =
    tyTxOutRefRef l == tyTxOutRefRef r
      && tyTxOutRefOut l == tyTxOutRefOut r

-- | Create a 'TypedScriptTxOut' from an existing 'TxOut' by checking the types of its parts.
typeScriptTxOut ::
  forall out m.
  ( FromData (DatumType out),
    ToData (DatumType out),
    MonadError ConnectionError m
  ) =>
  MultiPurposeScript out ->
  TxOutRef ->
  TxOut ->
  Datum ->
  m (TypedScriptTxOut out)
typeScriptTxOut tv txOutRef txOut datum = do
  case addressCredential (txOutAddress txOut) of
    PubKeyCredential _ ->
      throwError $ WrongOutType ExpectedScriptGotPubkey
    ScriptCredential _vh ->
      case txOutDatum txOut of
        OutputDatum d | datumHash datum == datumHash d -> do
          checkValidatorAddress tv (txOutAddress txOut)
          dsVal <- checkDatum tv datum
          pure $ TypedScriptTxOut @out txOut dsVal
        OutputDatumHash dh | datumHash datum == dh -> do
          checkValidatorAddress tv (txOutAddress txOut)
          dsVal <- checkDatum tv datum
          pure $ TypedScriptTxOut @out txOut dsVal
        _ -> throwError $ NoDatum txOutRef (datumHash datum)

-- | Create a 'TypedScriptTxOut' from an existing 'TxOut' by checking the types of its parts.
typeScriptTxOutRef ::
  forall out m.
  ( FromData (DatumType out),
    ToData (DatumType out),
    MonadError ConnectionError m
  ) =>
  MultiPurposeScript out ->
  TxOutRef ->
  TxOut ->
  Datum ->
  m (TypedScriptTxOutRef out)
typeScriptTxOutRef tv txOutRef txOut datum = do
  tyOut <- typeScriptTxOut tv txOutRef txOut datum
  pure $ TypedScriptTxOutRef txOutRef tyOut
