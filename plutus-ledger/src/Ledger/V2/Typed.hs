{-# LANGUAGE NamedFieldPuns #-}

module Ledger.V2.Typed
  ( validatorToTypedValidator,
    alwaysSucceedTypedValidator,
    alwaysFailTypedValidator,
    mkTypedValidator,
    mkTypedValidatorParam,
    makeTypedScriptTxOut,
    typeScriptTxOut,
    typeScriptTxOutRef,
    toCardanoAddressAny,
  )
where

import Control.Monad.Except (MonadError, throwError)
import Ledger.Data.CardanoAPI (datumHash)
import Ledger.V1.Typed (toCardanoAddressAny)
import Plutus.Script.Utils.V2
  ( ConnectionError (NoDatum, WrongCredentialType),
    DatumType,
    IsDataDatum,
    ToMintingPolicyHash (toMintingPolicyHash),
    ToScriptHash (toScriptHash),
    ToValidator (toValidator),
    ToValidatorHash (toValidatorHash),
    TypedScriptTxOut (TypedScriptTxOut),
    TypedScriptTxOutRef (TypedScriptTxOutRef),
    TypedValidator (TypedValidator),
    UntypedValidator,
    Validator,
    ValidatorType,
    Versioned (Versioned),
    WrongOutTypeError (ExpectedScriptGotPubkey),
    alwaysFailValidator,
    alwaysSucceedValidator,
    checkDatum,
    checkValidatorAddress,
    mkForwardingMintingPolicy,
    tvValidatorHash,
  )
import PlutusCore.Core (plcVersion100)
import PlutusCore.Default (DefaultUni)
import PlutusLedgerApi.Common (PlutusLedgerLanguage (PlutusV2))
import PlutusLedgerApi.V2
  ( Address (Address, addressCredential),
    Credential (PubKeyCredential, ScriptCredential),
    Datum (Datum),
    OutputDatum (OutputDatum, OutputDatumHash),
    ToData (toBuiltinData),
    TxOut (TxOut),
    TxOutRef,
    Value,
  )
import PlutusTx (CompiledCode, Lift, liftCode, unsafeApplyCode)

validatorToTypedValidator :: Validator -> TypedValidator a
validatorToTypedValidator val =
  TypedValidator val hsh fmp fmph lang
  where
    lang = PlutusV2
    hsh = toValidatorHash (Versioned val lang)
    fmp = mkForwardingMintingPolicy hsh
    fmph = toMintingPolicyHash (Versioned fmp lang)

alwaysSucceedTypedValidator :: TypedValidator a
alwaysSucceedTypedValidator = validatorToTypedValidator alwaysSucceedValidator

alwaysFailTypedValidator :: TypedValidator a
alwaysFailTypedValidator = validatorToTypedValidator alwaysFailValidator

mkTypedValidator ::
  CompiledCode (ValidatorType a) ->
  CompiledCode (ValidatorType a -> UntypedValidator) ->
  TypedValidator a
mkTypedValidator vc wrapper =
  validatorToTypedValidator $ toValidator $ wrapper `unsafeApplyCode` vc

mkTypedValidatorParam ::
  forall a param.
  (Lift DefaultUni param) =>
  CompiledCode (param -> ValidatorType a) ->
  CompiledCode (ValidatorType a -> UntypedValidator) ->
  param ->
  TypedValidator a
mkTypedValidatorParam vc wrapper param =
  mkTypedValidator (vc `unsafeApplyCode` liftCode plcVersion100 param) wrapper

makeTypedScriptTxOut ::
  (IsDataDatum out) =>
  TypedValidator out ->
  DatumType out ->
  Value ->
  TypedScriptTxOut out
makeTypedScriptTxOut (TypedValidator {tvValidatorHash}) dat val =
  TypedScriptTxOut
    ( TxOut
        (Address (ScriptCredential (toScriptHash tvValidatorHash)) Nothing)
        val
        (OutputDatumHash $ datumHash $ Datum $ toBuiltinData dat)
        Nothing
    )
    dat

typeScriptTxOut ::
  (IsDataDatum out, MonadError ConnectionError m) =>
  TypedValidator out ->
  TxOutRef ->
  TxOut ->
  Datum ->
  m (TypedScriptTxOut out)
typeScriptTxOut tv txOutRef txOut@(TxOut addr _ dat _) datum =
  case addressCredential addr of
    PubKeyCredential _ ->
      throwError $ WrongCredentialType ExpectedScriptGotPubkey
    ScriptCredential _vh ->
      case dat of
        OutputDatum d
          | datumHash datum == datumHash d ->
              checkValidatorAddress tv addr
                >> TypedScriptTxOut txOut <$> checkDatum tv datum
        OutputDatumHash dh
          | datumHash datum == dh ->
              checkValidatorAddress tv addr
                >> TypedScriptTxOut txOut <$> checkDatum tv datum
        _ -> throwError $ NoDatum txOutRef (datumHash datum)

typeScriptTxOutRef ::
  (IsDataDatum out, MonadError ConnectionError m) =>
  TypedValidator out ->
  TxOutRef ->
  TxOut ->
  Datum ->
  m (TypedScriptTxOutRef out)
typeScriptTxOutRef tv txOutRef txOut datum =
  TypedScriptTxOutRef txOutRef <$> typeScriptTxOut tv txOutRef txOut datum
