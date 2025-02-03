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
import Optics.Core (Getter, view)
import Plutus.Script.Utils.Scripts (
  Language (PlutusV3),
  MintingPolicy (MintingPolicy),
  MintingPolicyHash,
  Script (Script),
  ScriptHash,
  StakeValidator (StakeValidator),
  StakeValidatorHash,
  Validator (Validator),
  ValidatorHash (ValidatorHash),
  Versioned (Versioned),
  mintingPolicyHash,
  scriptCurrencySymbol,
  scriptHash,
  stakeValidatorHash,
  validatorHash,
 )
import PlutusLedgerApi.V3 qualified as Api
import PlutusTx.IsData (
  FromData,
  fromBuiltinData,
  unstableMakeIsData,
 )
import PlutusTx.Prelude (
  Bool (False),
  BuiltinByteString,
  BuiltinData,
  BuiltinString,
  Either (Left, Right),
  Integer,
  Maybe (Just, Nothing),
  any,
  check,
  either,
  maybe,
  return,
  trace,
  traceError,
  ($),
  (&&),
  (.),
  (<$>),
  (==),
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

type MintingScriptType a = Api.CurrencySymbol -> MintingRedeemerType a -> MintingTxInfo a -> Bool

type SpendingScriptType a =
  Api.TxOutRef -> Maybe (DatumType a) -> SpendingRedeemerType a -> SpendingTxInfo a -> Bool

type RewardingScriptType a = Api.Credential -> RewardingRedeemerType a -> RewardingTxInfo a -> Bool

type CertifyingScriptType a =
  Integer -> Api.TxCert -> CertifyingRedeemerType a -> CertifyingTxInfo a -> Bool

type VotingScriptType a = Api.Voter -> VotingRedeemerType a -> VotingTxInfo a -> Bool

type ProposingScriptType a =
  Integer -> Api.ProposalProcedure -> ProposingRedeemerType a -> ProposingTxInfo a -> Bool

data TypedScriptV3 a = TypedScriptV3
  { mintingTypedScript :: MintingScriptType a
  , spendingTypedScript :: SpendingScriptType a
  , rewardingTypedScript :: RewardingScriptType a
  , certifyingTypedScript :: CertifyingScriptType a
  , votingTypedScript :: VotingScriptType a
  , proposingTypedScript :: ProposingScriptType a
  }

instance Default (TypedScriptV3 a) where
  {-# INLINEABLE def #-}
  def =
    TypedScriptV3
      (\_ _ _ -> False)
      (\_ _ _ _ -> False)
      (\_ _ _ -> False)
      (\_ _ _ _ -> False)
      (\_ _ _ -> False)
      (\_ _ _ _ -> False)

-- | Adds (or overrides) the minting purpose to a V3 typed script
{-# INLINEABLE withMintingPurpose #-}
withMintingPurpose :: TypedScriptV3 a -> MintingScriptType a -> TypedScriptV3 a
withMintingPurpose ts ms = ts{mintingTypedScript = ms}

-- | Adds a minting constraint to an existing minting purpose
{-# INLINEABLE addMintingConstraint #-}
addMintingConstraint :: TypedScriptV3 a -> MintingScriptType a -> TypedScriptV3 a
addMintingConstraint ts ms = ts `withMintingPurpose` \cs red txInfo -> mintingTypedScript ts cs red txInfo && ms cs red txInfo

-- | Utility function to check that an input exists at a given script address
{-# INLINEABLE inputExistsAtScriptAddress #-}
inputExistsAtScriptAddress :: [Api.TxInInfo] -> BuiltinByteString -> Bool
inputExistsAtScriptAddress txInfo bs =
  any
    (== bs)
    [ h
    | Api.TxInInfo _ (Api.TxOut (Api.Address (Api.ScriptCredential (Api.ScriptHash h)) _) _ _ _) <- txInfo
    ]

-- | Minting policy that ensure a given validator is called in the transaction
{-# INLINEABLE withForwardingMintingPolicy #-}
withForwardingMintingPolicy
  :: TypedScriptV3 a -> ValidatorHash -> Getter (MintingTxInfo a) [Api.TxInInfo] -> TypedScriptV3 a
withForwardingMintingPolicy ts (ValidatorHash hash) l = ts `withMintingPurpose` \_ _ txInfo -> inputExistsAtScriptAddress (view l txInfo) hash

-- | Minting policy that ensure the own spending purpose check is called in the transaction
{-# INLINEABLE withForwardingMintingPolicyToOwn #-}
withForwardingMintingPolicyToOwn
  :: TypedScriptV3 a -> Getter (MintingTxInfo a) [Api.TxInInfo] -> TypedScriptV3 a
withForwardingMintingPolicyToOwn ts l =
  ts `withMintingPurpose` \(Api.CurrencySymbol cs) _ txInfo -> inputExistsAtScriptAddress (view l txInfo) cs

-- | Adds (or overrides) the spending purpose to a V3 typed script
{-# INLINEABLE withSpendingPurpose #-}
withSpendingPurpose :: TypedScriptV3 a -> SpendingScriptType a -> TypedScriptV3 a
withSpendingPurpose ts ss = ts{spendingTypedScript = ss}

{-# INLINEABLE withRewardingPurpose #-}
withRewardingPurpose :: TypedScriptV3 a -> RewardingScriptType a -> TypedScriptV3 a
withRewardingPurpose ts rs = ts{rewardingTypedScript = rs}

{-# INLINEABLE withCertifyingPurpose #-}
withCertifyingPurpose :: TypedScriptV3 a -> CertifyingScriptType a -> TypedScriptV3 a
withCertifyingPurpose ts cs = ts{certifyingTypedScript = cs}

{-# INLINEABLE withVotingPurpose #-}
withVotingPurpose :: TypedScriptV3 a -> VotingScriptType a -> TypedScriptV3 a
withVotingPurpose ts vs = ts{votingTypedScript = vs}

{-# INLINEABLE withProposingPurpose #-}
withProposingPurpose :: TypedScriptV3 a -> ProposingScriptType a -> TypedScriptV3 a
withProposingPurpose ts ps = ts{proposingTypedScript = ps}

data ScriptContextResolvedScriptInfo = ScriptContextResolvedScriptInfo
  { scriptContextTxInfo :: BuiltinData
  , scriptContextRedeemer :: BuiltinData
  , scriptContextScriptInfo :: Api.ScriptInfo
  }

unstableMakeIsData ''ScriptContextResolvedScriptInfo

type TypedScriptV3Constraints a =
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

multiPurposeToMintingPolicy :: MultiPurposeScript -> MintingPolicy
multiPurposeToMintingPolicy = MintingPolicy . getMultiPurposeScript

multiPurposeToValidator :: MultiPurposeScript -> Validator
multiPurposeToValidator = Validator . getMultiPurposeScript

multiPurposeToStakeValidator :: MultiPurposeScript -> StakeValidator
multiPurposeToStakeValidator = StakeValidator . getMultiPurposeScript

multiPurposeToScriptHash :: MultiPurposeScript -> ScriptHash
multiPurposeToScriptHash = scriptHash . (`Versioned` PlutusV3) . getMultiPurposeScript

multiPurposeToValidatorHash :: MultiPurposeScript -> ValidatorHash
multiPurposeToValidatorHash = validatorHash . (`Versioned` PlutusV3) . multiPurposeToValidator

multiPurposeToStakeValidatorHash :: MultiPurposeScript -> StakeValidatorHash
multiPurposeToStakeValidatorHash = stakeValidatorHash . (`Versioned` PlutusV3) . multiPurposeToStakeValidator

multiPurposeMintingPolicyHash :: MultiPurposeScript -> MintingPolicyHash
multiPurposeMintingPolicyHash = mintingPolicyHash . (`Versioned` PlutusV3) . multiPurposeToMintingPolicy

multiPurposeScriptCurrencySymbol :: MultiPurposeScript -> Api.CurrencySymbol
multiPurposeScriptCurrencySymbol = scriptCurrencySymbol . (`Versioned` PlutusV3) . multiPurposeToMintingPolicy

{-# INLINEABLE fromBuiltinDataEither #-}
fromBuiltinDataEither :: (FromData a) => BuiltinString -> BuiltinData -> Either BuiltinString a
fromBuiltinDataEither err = maybe (Left err) Right . fromBuiltinData

{-# INLINEABLE toValidator #-}
toValidator :: (TypedScriptV3Constraints a) => TypedScriptV3 a -> MultiPurposeScript
toValidator (TypedScriptV3{..}) =
  MultiPurposeScript $ Script $ Api.serialiseCompiledCode $$(compile [||validatorCode||])
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
            $ mintingTypedScript cur red txInfo
        Api.SpendingScript oRef mDat -> do
          red <- fromBuiltinDataEither "Error when deserializing the spending redeemer" scriptContextRedeemer
          txInfo <- fromBuiltinDataEither "Error when deserializing the spending tx info" scriptContextTxInfo
          mResolvedDat <- trace "Spending redeemer successfully deserialized" $ case mDat of
            Nothing -> return Nothing
            Just (Api.Datum bDat) -> Just <$> fromBuiltinDataEither "Error when deserializing the datum" bDat
          return
            $ trace "Running the validator with the Spending purpose"
            $ spendingTypedScript oRef mResolvedDat red txInfo
        Api.RewardingScript cred -> do
          red <- fromBuiltinDataEither "Error when deserializing the rewarding redeemer" scriptContextRedeemer
          txInfo <- fromBuiltinDataEither "Error when deserializing the rewarding tx info" scriptContextTxInfo
          return
            $ trace "Running the validator with the Rewarding purpose"
            $ rewardingTypedScript cred red txInfo
        Api.CertifyingScript i cert -> do
          red <-
            fromBuiltinDataEither "Error when deserializing the certifying redeemer" scriptContextRedeemer
          txInfo <-
            fromBuiltinDataEither "Error when deserializing the certifying tx info" scriptContextTxInfo
          return
            $ trace "Running the validator with the Certifying purpose"
            $ certifyingTypedScript i cert red txInfo
        Api.VotingScript voter -> do
          red <- fromBuiltinDataEither "Error when deserializing the voting redeemer" scriptContextRedeemer
          txInfo <- fromBuiltinDataEither "Error when deserializing the voting tx info" scriptContextTxInfo
          return
            $ trace "Running the validator with the Voting purpose"
            $ votingTypedScript voter red txInfo
        Api.ProposingScript i prop -> do
          red <- fromBuiltinDataEither "Error when deserializing the proposing redeemer" scriptContextRedeemer
          txInfo <- fromBuiltinDataEither "Error when deserializing the proposing tx info" scriptContextTxInfo
          return
            $ trace "Running the validator with the Proposing purpose"
            $ proposingTypedScript i prop red txInfo
