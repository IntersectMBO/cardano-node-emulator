{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ledger.Typed.Scripts.V1
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

import Cardano.Api qualified as C.Api
import Control.Monad.Except (MonadError, throwError)
import Ledger.Data.CardanoAPI (datumHash)
import Ledger.Scripts (ToCardanoAddress (toCardanoAddress))
import Plutus.Script.Utils.Address (ToAddress (toAddress), ToCredential (toCredential))
import Plutus.Script.Utils.Scripts
  ( Language (PlutusV1),
    Script,
    ToMintingPolicyHash (toMintingPolicyHash),
    ToScriptHash (toScriptHash),
    ToValidator (toValidator),
    ToValidatorHash (toValidatorHash),
    ToVersioned (toVersioned),
    Validator,
    Versioned (Versioned),
  )
import Plutus.Script.Utils.V1.Generators
  ( alwaysFailValidator,
    alwaysSucceedValidator,
    mkForwardingMintingPolicy,
  )
import Plutus.Script.Utils.V1.Scripts (UntypedValidator)
import Plutus.Script.Utils.V1.Typed as Typed
import PlutusCore.Default (DefaultUni)
import PlutusCore.Version (plcVersion100)
import PlutusLedgerApi.V1
  ( Address (Address, addressCredential),
    Credential (PubKeyCredential, ScriptCredential),
    Datum (Datum),
    ToData (toBuiltinData),
    TxOut (TxOut),
    TxOutRef,
    Value,
  )
import PlutusTx (CompiledCode, Lift, liftCode, unsafeApplyCode)

instance ToScriptHash (TypedValidator a) where
  toScriptHash = toScriptHash . toVersioned @Script

instance ToValidatorHash (TypedValidator a) where
  toValidatorHash = toValidatorHash . toVersioned @Validator

instance ToCredential (TypedValidator a) where
  toCredential = toCredential . toVersioned @Script

instance ToAddress (TypedValidator a) where
  toAddress = toAddress . toVersioned @Script

instance ToCardanoAddress (TypedValidator a) where
  toCardanoAddress networkId = toCardanoAddress networkId . toVersioned @Script

toCardanoAddressAny :: C.Api.NetworkId -> TypedValidator a -> C.Api.AddressAny
toCardanoAddressAny nid tv =
  case toCardanoAddress nid tv of
    C.Api.AddressInEra C.Api.ShelleyAddressInEra {} addr -> C.Api.AddressShelley addr
    C.Api.AddressInEra C.Api.ByronAddressInAnyEra {} addr -> C.Api.AddressByron addr

validatorToTypedValidator :: Validator -> TypedValidator a
validatorToTypedValidator val =
  TypedValidator val hsh fmp fmph lang
  where
    lang = PlutusV1
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
        (Just (datumHash $ Datum $ toBuiltinData dat))
    )
    dat

typeScriptTxOut ::
  (IsDataDatum out, MonadError ConnectionError m) =>
  TypedValidator out ->
  TxOutRef ->
  TxOut ->
  Datum ->
  m (TypedScriptTxOut out)
typeScriptTxOut tv txOutRef txOut@(TxOut addr _ dHash) datum =
  case addressCredential addr of
    PubKeyCredential _ ->
      throwError $ WrongCredentialType ExpectedScriptGotPubkey
    ScriptCredential _vh ->
      case dHash of
        Just dh
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
