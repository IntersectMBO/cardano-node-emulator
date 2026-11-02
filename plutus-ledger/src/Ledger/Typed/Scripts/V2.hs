{-# LANGUAGE NamedFieldPuns #-}

module Ledger.Typed.Scripts.V2
  ( module Typed,
    validatorToTypedValidator,
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
import Ledger.Scripts ()
import Ledger.Typed.Scripts.V1 (toCardanoAddressAny)
import Plutus.Script.Utils.Scripts
  ( Language (PlutusV2),
    ToMintingPolicyHash (toMintingPolicyHash),
    ToScriptHash (toScriptHash),
    ToValidator (toValidator),
    ToValidatorHash (toValidatorHash),
    Validator,
    Versioned (Versioned),
  )
import Plutus.Script.Utils.V2.Generators
  ( alwaysFailValidator,
    alwaysSucceedValidator,
    mkForwardingMintingPolicy,
  )
import Plutus.Script.Utils.V2.Scripts (UntypedValidator)
import Plutus.Script.Utils.V2.Typed as Typed
import PlutusCore.Core (plcVersion100)
import PlutusCore.Default (DefaultUni)
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
