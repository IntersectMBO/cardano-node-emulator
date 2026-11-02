{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutus.Script.Utils.V2.Typed
  ( ValidatorType,
    ConnectionError (..),
    WrongOutTypeError (..),
    checkValidatorAddress,
    checkRedeemer,
    checkDatum,
    TypedScriptTxOut (..),
    TypedScriptTxOutRef (..),
    ValidatorTypes (..),
    TypedValidator (..),
    generalise,
    Any,
    IsDataDatum,
  )
where

import Control.Monad.Except (MonadError, throwError)
import Data.Kind (Type)
import Plutus.Script.Utils.V1.Typed
  ( Any,
    ConnectionError
      ( NoDatum,
        UnknownRef,
        WrongCredentialType,
        WrongDatumType,
        WrongRedeemerType,
        WrongValidatorHash,
        WrongValidatorType
      ),
    IsDataDatum,
    TypedValidator
      ( TypedValidator,
        tvForwardingMintingPolicy,
        tvForwardingMintingPolicyHash,
        tvLanguage,
        tvValidator,
        tvValidatorHash
      ),
    ValidatorTypes (DatumType, RedeemerType),
    WrongOutTypeError (ExpectedPubkeyGotScript, ExpectedScriptGotPubkey),
    checkValidatorAddress,
    generalise,
  )
import PlutusLedgerApi.V2
  ( Datum (Datum),
    FromData,
    Redeemer (Redeemer),
    ScriptContext,
    TxOut,
    TxOutRef,
  )
import PlutusTx (FromData (fromBuiltinData))

-- | The type of validators for the given connection type.
type ValidatorType (a :: Type) = DatumType a -> RedeemerType a -> ScriptContext -> Bool

-- | Checks that the given redeemer script has the right type.
checkRedeemer ::
  forall inn m.
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
  forall a m.
  (FromData (DatumType a), MonadError ConnectionError m) =>
  TypedValidator a ->
  Datum ->
  m (DatumType a)
checkDatum _ (Datum d) =
  case fromBuiltinData d of
    Just v -> pure v
    Nothing -> throwError $ WrongDatumType d

-- | A 'TxOut' tagged by a phantom type: and the connection type of the output.
data TypedScriptTxOut a
  = (IsDataDatum a) =>
  TypedScriptTxOut
  { tyTxOutTxOut :: TxOut,
    tyTxOutData :: DatumType a
  }

deriving stock instance (Eq (DatumType a)) => Eq (TypedScriptTxOut a)

-- | A 'TxOutRef' tagged by a phantom type: and the connection type of the output.
data TypedScriptTxOutRef a = TypedScriptTxOutRef
  { tyTxOutRefRef :: TxOutRef,
    tyTxOutRefOut :: TypedScriptTxOut a
  }

deriving stock instance (Eq (DatumType a)) => Eq (TypedScriptTxOutRef a)
