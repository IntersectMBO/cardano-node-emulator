{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ledger.Index.Internal where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.Binary qualified as CBOR
import Cardano.Ledger.Alonzo.Scripts (AsIx, ExUnits, PlutusPurpose)
import Cardano.Ledger.Alonzo.Tx (AlonzoTx (AlonzoTx), IsValid (IsValid))
import Cardano.Ledger.Core (Tx)
import Cardano.Ledger.Shelley.API (Validated, extractTx)
import Codec.Serialise (Serialise (..))
import Control.Lens (makePrisms)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Map qualified as Map
import Data.Text (Text)
import GHC.Generics (Generic)
import Ledger.Orphans ()
import Ledger.Tx.CardanoAPI.Internal (CardanoTx, EmulatorEra, pattern CardanoEmulatorEraTx)
import PlutusLedgerApi.V1.Scripts qualified as Scripts
import Prettyprinter (Pretty (..), hang, vsep, (<+>))
import Prettyprinter.Extras (PrettyShow (..))
import Prettyprinter.Util (reflow)
import Prelude hiding (lookup)

-- | A transaction on the blockchain.
-- Invalid transactions are still put on the chain to be able to collect fees.
newtype OnChainTx = OnChainTx {getOnChainTx :: Validated (Tx EmulatorEra)}
  deriving (Eq, Show, Generic)

instance Serialise OnChainTx where
  encode = CBOR.toCBOR . extractTx . getOnChainTx -- For blockID
  decode = fail "Not allowed to use `decode` on `OnChainTx`" -- Unused

eitherTx :: (CardanoTx -> r) -> (CardanoTx -> r) -> OnChainTx -> r
eitherTx ifInvalid ifValid (extractTx . getOnChainTx -> tx@(AlonzoTx _ _ (IsValid isValid) _)) =
  let ctx = CardanoEmulatorEraTx (C.ShelleyTx C.ShelleyBasedEraConway tx)
   in if isValid then ifValid ctx else ifInvalid ctx

unOnChain :: OnChainTx -> CardanoTx
unOnChain = eitherTx id id

-- | The UTxOs of a blockchain indexed by their references.
type UtxoIndex = C.UTxO C.ConwayEra

-- | A reason why a transaction is invalid.
data ValidationError
  = -- | The transaction output consumed by a transaction input could not be found (either because it was already spent, or because
    -- there was no transaction with the given hash on the blockchain).
    TxOutRefNotFound C.TxIn
  | -- | For pay-to-script outputs: evaluation of the validator script failed.
    ScriptFailure Scripts.ScriptError
  | -- | An error from Cardano.Ledger validation
    CardanoLedgerValidationError Text
  | -- | Balancing failed, it needed more than the maximum number of collateral inputs
    MaxCollateralInputsExceeded
  deriving (Eq, Show, Generic)

makePrisms ''ValidationError

instance FromJSON ValidationError

instance ToJSON ValidationError

deriving via (PrettyShow ValidationError) instance Pretty ValidationError

data ValidationPhase = Phase1 | Phase2 deriving (Eq, Show, Generic, FromJSON, ToJSON)

deriving via (PrettyShow ValidationPhase) instance Pretty ValidationPhase

type ValidationErrorInPhase = (ValidationPhase, ValidationError)

type ValidationSuccess = (RedeemerReport, Validated (Tx EmulatorEra))

type RedeemerReport = Map.Map (PlutusPurpose AsIx EmulatorEra) ([Text], ExUnits)

data ValidationResult
  = -- | A transaction failed to validate in phase 1.
    FailPhase1 !CardanoTx !ValidationError
  | -- | A transaction failed to validate in phase 2. The @Value@ indicates the amount of collateral stored in the transaction.
    FailPhase2 !OnChainTx !ValidationError !C.Value
  | Success !OnChainTx !RedeemerReport
  deriving stock (Eq, Show, Generic)

makePrisms ''ValidationResult

data ValidationResultSimple
  = ValidationFailPhase1 !CardanoTx !ValidationError
  | ValidationFailPhase2 !CardanoTx !ValidationError !C.Value
  | ValidationSuccess !CardanoTx
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

toValidationResultSimple :: ValidationResult -> ValidationResultSimple
toValidationResultSimple (FailPhase1 tx err) = ValidationFailPhase1 tx err
toValidationResultSimple (FailPhase2 vtx err val) = ValidationFailPhase2 (unOnChain vtx) err val
toValidationResultSimple (Success vtx _) = ValidationSuccess (unOnChain vtx)

instance ToJSON ValidationResult where
  toJSON = toJSON . toValidationResultSimple

instance FromJSON ValidationResult where
  parseJSON = mempty -- Always fail, this instance isn't really used, but required by pab's logging framework.

instance Pretty ValidationResult where
  pretty res =
    hang 2 $
      vsep $
        case res of
          FailPhase1 _ err -> "Validation failed in phase 1:" <+> pretty err
          FailPhase2 _ err _ -> "Validation failed in phase 2:" <+> pretty err
          Success {} -> "Validation success"
          : fmap reflow (getEvaluationLogs res)

cardanoTxFromValidationResult :: ValidationResult -> CardanoTx
cardanoTxFromValidationResult (FailPhase1 tx _) = tx
cardanoTxFromValidationResult (FailPhase2 vtx _ _) = unOnChain vtx
cardanoTxFromValidationResult (Success vtx _) = unOnChain vtx

toOnChain :: ValidationResult -> Maybe OnChainTx
toOnChain (Success tx _) = Just tx
toOnChain (FailPhase2 tx _ _) = Just tx
toOnChain _ = Nothing

-- | Get logs from evaluating plutus scripts.
getEvaluationLogs :: ValidationResult -> [Text]
getEvaluationLogs = \case
  Success _ r -> concatMap (fst . snd) $ Map.toList r
  FailPhase1 _ err -> logs err
  FailPhase2 _ err _ -> logs err
  where
    logs = \case
      ScriptFailure (Scripts.EvaluationError msgs _) -> msgs
      _ -> []
