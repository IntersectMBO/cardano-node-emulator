{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Plutus.Script.Utils.V3.Typed.Scripts (
  module Plutus.Script.Utils.V3.Typed.Scripts.MonetaryPolicies,
  module Plutus.Script.Utils.V3.Typed.Scripts.StakeValidators,
  module Plutus.Script.Utils.V3.Typed.Scripts.Validators,
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

import Control.Monad.Except (MonadError (throwError))
import Plutus.Script.Utils.Scripts (MintingPolicy, StakeValidator, Validator, datumHash)
import Plutus.Script.Utils.V3.Typed.Scripts.MonetaryPolicies hiding (forwardToValidator)
import Plutus.Script.Utils.V3.Typed.Scripts.StakeValidators hiding (forwardToValidator)
import Plutus.Script.Utils.V3.Typed.Scripts.Validators
import PlutusLedgerApi.V3 (
  Credential (PubKeyCredential, ScriptCredential),
  Datum,
  FromData,
  OutputDatum (OutputDatum, OutputDatumHash),
  ToData (..),
  TxOut (txOutAddress, txOutDatum),
  TxOutRef,
  addressCredential,
 )

--

-- | A 'TxOut' tagged by a phantom type: and the connection type of the output.
data TypedScriptTxOut a = (FromData (DatumType a), ToData (DatumType a)) =>
  TypedScriptTxOut
  { tyTxOutTxOut :: TxOut
  , tyTxOutData :: DatumType a
  }

instance (Eq (DatumType a)) => Eq (TypedScriptTxOut a) where
  l == r =
    tyTxOutTxOut l == tyTxOutTxOut r
      && tyTxOutData l == tyTxOutData r

-- | A 'TxOutRef' tagged by a phantom type: and the connection type of the output.
data TypedScriptTxOutRef a = TypedScriptTxOutRef
  { tyTxOutRefRef :: TxOutRef
  , tyTxOutRefOut :: TypedScriptTxOut a
  }

instance (Eq (DatumType a)) => Eq (TypedScriptTxOutRef a) where
  l == r =
    tyTxOutRefRef l == tyTxOutRefRef r
      && tyTxOutRefOut l == tyTxOutRefOut r

-- | Create a 'TypedScriptTxOut' from an existing 'TxOut' by checking the types of its parts.
typeScriptTxOut
  :: forall out m
   . ( FromData (DatumType out)
     , ToData (DatumType out)
     , MonadError ConnectionError m
     )
  => TypedValidator out
  -> TxOutRef
  -> TxOut
  -> Datum
  -> m (TypedScriptTxOut out)
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
typeScriptTxOutRef
  :: forall out m
   . ( FromData (DatumType out)
     , ToData (DatumType out)
     , MonadError ConnectionError m
     )
  => TypedValidator out
  -> TxOutRef
  -> TxOut
  -> Datum
  -> m (TypedScriptTxOutRef out)
typeScriptTxOutRef tv txOutRef txOut datum = do
  tyOut <- typeScriptTxOut tv txOutRef txOut datum
  pure $ TypedScriptTxOutRef txOutRef tyOut
