{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Plutus.Script.Utils.V3.Typed.Script.MultiPurpose where

import Codec.Serialise (Serialise)
import Data.Default (
  Default,
  def,
 )
import GHC.Generics (Generic)
import Plutus.Script.Utils.Scripts (
  Language (PlutusV3),
  Script (Script),
  Versioned (Versioned),
 )
import PlutusLedgerApi.V3 qualified as Api
import PlutusTx.IsData (
  FromData,
  fromBuiltinData,
  unstableMakeIsData,
 )
import PlutusTx.Prelude (
  Bool (False),
  BuiltinData,
  BuiltinString,
  Either (Left, Right),
  Integer,
  Maybe (Just, Nothing),
  check,
  either,
  maybe,
  return,
  trace,
  traceError,
  ($),
  (.),
  (<$>),
 )
import PlutusTx.TH (compile)
import Prettyprinter (Pretty)
import Prettyprinter.Extras (PrettyShow (PrettyShow))
import Prelude (Eq, Ord, Show (show))

class ValidatorTypes a where
  -- Minting purpose type variables with default
  type MintingRedeemerType a
  type MintingTxInfo a

  type MintingRedeemerType a = ()
  type MintingTxInfo a = Api.TxInfo

  -- Spending purpose type variables with default
  type SpendingRedeemerType a
  type SpendingTxInfo a
  type DatumType a

  type SpendingRedeemerType a = ()
  type SpendingTxInfo a = Api.TxInfo
  type DatumType a = ()

  -- Rewarding purpose type variables with default
  type RewardingRedeemerType a
  type RewardingTxInfo a

  type RewardingRedeemerType a = ()
  type RewardingTxInfo a = Api.TxInfo

  -- Certifying purpose type variables with default
  type CertifyingRedeemerType a
  type CertifyingTxInfo a

  type CertifyingRedeemerType a = ()
  type CertifyingTxInfo a = Api.TxInfo

  -- Voting purpose type variables with default
  type VotingRedeemerType a
  type VotingTxInfo a

  type VotingRedeemerType a = ()
  type VotingTxInfo a = Api.TxInfo

  -- Proposing purpose type variables with default
  type ProposingRedeemerType a
  type ProposingTxInfo a

  type ProposingRedeemerType a = ()
  type ProposingTxInfo a = Api.TxInfo

data TypedValidatorV3 a = TypedValidatorV3
  { mintingTypedValidator :: Api.CurrencySymbol -> MintingRedeemerType a -> MintingTxInfo a -> Bool
  , spendingTypedValidator
      :: Api.TxOutRef
      -> Maybe (DatumType a)
      -> SpendingRedeemerType a
      -> SpendingTxInfo a
      -> Bool
  , rewardingTypedValidator :: Api.Credential -> RewardingRedeemerType a -> RewardingTxInfo a -> Bool
  , certifyingTypedValidator
      :: Integer
      -> Api.TxCert
      -> CertifyingRedeemerType a
      -> CertifyingTxInfo a
      -> Bool
  , votingTypedValidator :: Api.Voter -> VotingRedeemerType a -> VotingTxInfo a -> Bool
  , proposingTypedValidator
      :: Integer
      -> Api.ProposalProcedure
      -> ProposingRedeemerType a
      -> ProposingTxInfo a
      -> Bool
  }

instance Default (TypedValidatorV3 a) where
  def =
    TypedValidatorV3
      (\_ _ _ -> False)
      (\_ _ _ _ -> False)
      (\_ _ _ -> False)
      (\_ _ _ _ -> False)
      (\_ _ _ -> False)
      (\_ _ _ _ -> False)

data ScriptContextResolvedScriptInfo = ScriptContextResolvedScriptInfo
  { scriptContextTxInfo :: BuiltinData
  , scriptContextRedeemer :: BuiltinData
  , scriptContextScriptInfo :: Api.ScriptInfo
  }

unstableMakeIsData ''ScriptContextResolvedScriptInfo

type TypedValidatorV3Constraints a =
  ( ValidatorTypes a
  , FromData (MintingRedeemerType a)
  , FromData (MintingTxInfo a)
  , FromData (SpendingRedeemerType a)
  , FromData (SpendingTxInfo a)
  , FromData (DatumType a)
  , FromData (RewardingRedeemerType a)
  , FromData (RewardingTxInfo a)
  , FromData (CertifyingRedeemerType a)
  , FromData (CertifyingTxInfo a)
  , FromData (VotingRedeemerType a)
  , FromData (VotingTxInfo a)
  , FromData (ProposingRedeemerType a)
  , FromData (ProposingTxInfo a)
  )

newtype MultiPurposeScript = MultiPurposeScript {getMultiPurposeScript :: Script}
  deriving stock (Generic)
  deriving newtype (Eq, Ord, Serialise)
  deriving (Pretty) via (PrettyShow MultiPurposeScript)

instance Show MultiPurposeScript where
  show _ = "Multi purpose script { <script> }"

{-# INLINEABLE fromBuiltinDataEither #-}
fromBuiltinDataEither :: (FromData a) => BuiltinString -> BuiltinData -> Either BuiltinString a
fromBuiltinDataEither err = maybe (Left err) Right . fromBuiltinData

{-# INLINEABLE toValidator #-}
toValidator :: (TypedValidatorV3Constraints a) => TypedValidatorV3 a -> Versioned MultiPurposeScript
toValidator (TypedValidatorV3{..}) =
  Versioned
    (MultiPurposeScript $ Script $ Api.serialiseCompiledCode $$(compile [||validatorCode||]))
    PlutusV3
  where
    validatorCode dat = either traceError check $ do
      ScriptContextResolvedScriptInfo{..} <-
        fromBuiltinDataEither "Error when deserializing the script info" dat
      trace "Script info successfully deserialized" $ case scriptContextScriptInfo of
        Api.MintingScript cur -> do
          red <- fromBuiltinDataEither "Error when deserializing the minting redeemer" scriptContextRedeemer
          txInfo <- fromBuiltinDataEither "Error when deserializing the minting tx info" scriptContextTxInfo
          return
            $ trace "Running the validator with the Minting purpose"
            $ mintingTypedValidator cur red txInfo
        Api.SpendingScript oRef mDat -> do
          red <- fromBuiltinDataEither "Error when deserializing the spending redeemer" scriptContextRedeemer
          txInfo <- fromBuiltinDataEither "Error when deserializing the spending tx info" scriptContextTxInfo
          mResolvedDat <- trace "Spending redeemer successfully deserialized" $ case mDat of
            Nothing -> return Nothing
            Just (Api.Datum bDat) -> Just <$> fromBuiltinDataEither "Error when deserializing the datum" bDat
          return
            $ trace "Running the validator with the Spending purpose"
            $ spendingTypedValidator oRef mResolvedDat red txInfo
        Api.RewardingScript cred -> do
          red <- fromBuiltinDataEither "Error when deserializing the rewarding redeemer" scriptContextRedeemer
          txInfo <- fromBuiltinDataEither "Error when deserializing the rewarding tx info" scriptContextTxInfo
          return
            $ trace "Running the validator with the Rewarding purpose"
            $ rewardingTypedValidator cred red txInfo
        Api.CertifyingScript i cert -> do
          red <-
            fromBuiltinDataEither "Error when deserializing the certifying redeemer" scriptContextRedeemer
          txInfo <-
            fromBuiltinDataEither "Error when deserializing the certifying tx info" scriptContextTxInfo
          return
            $ trace "Running the validator with the Certifying purpose"
            $ certifyingTypedValidator i cert red txInfo
        Api.VotingScript voter -> do
          red <- fromBuiltinDataEither "Error when deserializing the voting redeemer" scriptContextRedeemer
          txInfo <- fromBuiltinDataEither "Error when deserializing the voting tx info" scriptContextTxInfo
          return
            $ trace "Running the validator with the Voting purpose"
            $ votingTypedValidator voter red txInfo
        Api.ProposingScript i prop -> do
          red <- fromBuiltinDataEither "Error when deserializing the proposing redeemer" scriptContextRedeemer
          txInfo <- fromBuiltinDataEither "Error when deserializing the proposing tx info" scriptContextTxInfo
          return
            $ trace "Running the validator with the Proposing purpose"
            $ proposingTypedValidator i prop red txInfo
