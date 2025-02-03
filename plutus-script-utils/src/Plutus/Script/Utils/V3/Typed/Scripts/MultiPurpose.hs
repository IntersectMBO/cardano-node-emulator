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
import Data.Default (Default, def)
import GHC.Generics (Generic)
import Optics.Core (Getter, to, view)
import Plutus.Script.Utils.Scripts (
  Language (PlutusV3),
  MintingPolicy (MintingPolicy),
  MintingPolicyHash (MintingPolicyHash),
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
import PlutusTx.AssocMap (member)
import PlutusTx.Builtins.Internal (BuiltinUnit (BuiltinUnit))
import PlutusTx.IsData (
  FromData,
  fromBuiltinData,
  unstableMakeIsData,
 )
import PlutusTx.Prelude (
  Bool (False),
  BuiltinByteString,
  BuiltinData,
  Either (Left, Right),
  Integer,
  Maybe (Just, Nothing),
  any,
  check,
  either,
  elem,
  id,
  maybe,
  return,
  trace,
  traceError,
  ($),
  (&&),
  (.),
  (<$>),
  (<>),
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

data TypedMultiPurposeScript a = TypedMultiPurposeScript
  { mintingTypedScript :: MintingScriptType a
  , spendingTypedScript :: SpendingScriptType a
  , rewardingTypedScript :: RewardingScriptType a
  , certifyingTypedScript :: CertifyingScriptType a
  , votingTypedScript :: VotingScriptType a
  , proposingTypedScript :: ProposingScriptType a
  }

instance Default (TypedMultiPurposeScript a) where
  {-# INLINEABLE def #-}
  def =
    TypedMultiPurposeScript
      (\_ _ _ -> False)
      (\_ _ _ _ -> False)
      (\_ _ _ -> False)
      (\_ _ _ _ -> False)
      (\_ _ _ -> False)
      (\_ _ _ _ -> False)

-- * Working with the Minting purpose of a multipurpose script

-- | Adds (or overrides) the minting purpose to a V3 typed script
{-# INLINEABLE withMintingPurpose #-}
withMintingPurpose :: TypedMultiPurposeScript a -> MintingScriptType a -> TypedMultiPurposeScript a
withMintingPurpose ts ms = ts{mintingTypedScript = ms}

-- | Adds a minting constraint to an existing minting purpose
{-# INLINEABLE addMintingConstraint #-}
addMintingConstraint
  :: TypedMultiPurposeScript a -> MintingScriptType a -> TypedMultiPurposeScript a
addMintingConstraint ts ms = ts `withMintingPurpose` \cs red txInfo -> mintingTypedScript ts cs red txInfo && ms cs red txInfo

-- | Utility function to check that an input exists at a given script address
{-# INLINEABLE inputExistsAtScriptAddress #-}
inputExistsAtScriptAddress :: [Api.TxInInfo] -> BuiltinByteString -> Bool
inputExistsAtScriptAddress txInfo bs =
  bs
    `elem` [ h
           | Api.TxInInfo _ (Api.TxOut (Api.Address (Api.ScriptCredential (Api.ScriptHash h)) _) _ _ _) <- txInfo
           ]

-- | Getting the inputs from a TxInfo
{-# INLINEABLE txInfoInputsG #-}
txInfoInputsG :: Getter Api.TxInfo [Api.TxInInfo]
txInfoInputsG = to Api.txInfoInputs

-- | Minting policy that ensure a given validator is called in the transaction
{-# INLINEABLE withForwardingMintingScript #-}
withForwardingMintingScript
  :: TypedMultiPurposeScript a
  -> ValidatorHash
  -> Getter (MintingTxInfo a) [Api.TxInInfo]
  -> TypedMultiPurposeScript a
withForwardingMintingScript ts (ValidatorHash hash) getter =
  ts `withMintingPurpose` \_ _ txInfo -> inputExistsAtScriptAddress (view getter txInfo) hash

-- | Minting policy that ensure the own spending purpose check is called in the transaction
{-# INLINEABLE withOwnForwardingMintingScript #-}
withOwnForwardingMintingScript
  :: TypedMultiPurposeScript a -> Getter (MintingTxInfo a) [Api.TxInInfo] -> TypedMultiPurposeScript a
withOwnForwardingMintingScript ts getter =
  ts `withMintingPurpose` \(Api.CurrencySymbol cs) _ txInfo -> inputExistsAtScriptAddress (view getter txInfo) cs

-- * Working with the Spending purpose of a multipurpose script

-- | Adds (or overrides) the spending purpose to a V3 typed script
{-# INLINEABLE withSpendingPurpose #-}
withSpendingPurpose
  :: TypedMultiPurposeScript a -> SpendingScriptType a -> TypedMultiPurposeScript a
withSpendingPurpose ts ss = ts{spendingTypedScript = ss}

-- | Adds a spending constraint to an existing spending purpose
{-# INLINEABLE addSpendingConstraint #-}
addSpendingConstraint
  :: TypedMultiPurposeScript a -> SpendingScriptType a -> TypedMultiPurposeScript a
addSpendingConstraint ts ss =
  ts `withSpendingPurpose` \oRef mDat red txInfo -> spendingTypedScript ts oRef mDat red txInfo && ss oRef mDat red txInfo

-- | Getting the minted value from a TxInfo
{-# INLINEABLE txInfoMintValueG #-}
txInfoMintValueG :: Getter Api.TxInfo Api.Value
txInfoMintValueG = to Api.txInfoMint

-- | Spending purpose that ensures a given minting script is invoked in the transaction
{-# INLINEABLE withForwardSpendingScript #-}
withForwardSpendingScript
  :: TypedMultiPurposeScript a
  -> MintingPolicyHash
  -> Getter (SpendingTxInfo a) Api.Value
  -> TypedMultiPurposeScript a
withForwardSpendingScript ts (MintingPolicyHash hash) getter =
  ts `withSpendingPurpose` \_ _ _ txInfo -> Api.CurrencySymbol hash `member` Api.getValue (view getter txInfo)

-- | Spending purpose that ensures the own minting purpose is invoked in the transaction
{-# INLINEABLE withOwnForwardSpendingScript #-}
withOwnForwardSpendingScript
  :: TypedMultiPurposeScript a
  -> Getter (SpendingTxInfo a) Api.Value
  -> Getter (SpendingTxInfo a) [Api.TxInInfo]
  -> TypedMultiPurposeScript a
withOwnForwardSpendingScript ts mintedValueGetter inputsGetter =
  ts `withSpendingPurpose` \oRef _ _ txInfo ->
    any
      ((`member` Api.getValue (view mintedValueGetter txInfo)) . Api.CurrencySymbol)
      [ h
      | Api.TxInInfo ref (Api.TxOut (Api.Address (Api.ScriptCredential (Api.ScriptHash h)) _) _ _ _) <-
          view inputsGetter txInfo
      , ref == oRef
      ]

-- * Working with the Rewarding purpose of a multipurpose script

{-# INLINEABLE withRewardingPurpose #-}
withRewardingPurpose
  :: TypedMultiPurposeScript a -> RewardingScriptType a -> TypedMultiPurposeScript a
withRewardingPurpose ts rs = ts{rewardingTypedScript = rs}

{-# INLINEABLE withCertifyingPurpose #-}
withCertifyingPurpose
  :: TypedMultiPurposeScript a -> CertifyingScriptType a -> TypedMultiPurposeScript a
withCertifyingPurpose ts cs = ts{certifyingTypedScript = cs}

{-# INLINEABLE withVotingPurpose #-}
withVotingPurpose :: TypedMultiPurposeScript a -> VotingScriptType a -> TypedMultiPurposeScript a
withVotingPurpose ts vs = ts{votingTypedScript = vs}

{-# INLINEABLE withProposingPurpose #-}
withProposingPurpose
  :: TypedMultiPurposeScript a -> ProposingScriptType a -> TypedMultiPurposeScript a
withProposingPurpose ts ps = ts{proposingTypedScript = ps}

data ScriptContextResolvedScriptInfo = ScriptContextResolvedScriptInfo
  { scriptContextTxInfo :: BuiltinData
  , scriptContextRedeemer :: BuiltinData
  , scriptContextScriptInfo :: Api.ScriptInfo
  }

unstableMakeIsData ''ScriptContextResolvedScriptInfo

type TypedMultiPurposeScriptConstraints a =
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

type UntypedMultiPurposeScript = BuiltinData -> BuiltinUnit

compileUntypedMultiPurposeScript :: UntypedMultiPurposeScript -> MultiPurposeScript
compileUntypedMultiPurposeScript script = MultiPurposeScript $ Script $ Api.serialiseCompiledCode $$(compile [||script||])

{-# INLINEABLE typedToUntypedMultiPurposeScript #-}
typedToUntypedMultiPurposeScript
  :: (TypedMultiPurposeScriptConstraints a) => TypedMultiPurposeScript a -> UntypedMultiPurposeScript
typedToUntypedMultiPurposeScript TypedMultiPurposeScript{..} dat = either traceError check $ do
  ScriptContextResolvedScriptInfo{..} <- fromBuiltinDataEither "script info" dat
  case scriptContextScriptInfo of
    Api.MintingScript cur -> do
      (red, txInfo) <- deserializeContext "minting" scriptContextRedeemer scriptContextTxInfo
      return $ traceRunning "Minting" $ mintingTypedScript cur red txInfo
    Api.SpendingScript oRef mDat -> do
      (red, txInfo) <- deserializeContext "spending" scriptContextRedeemer scriptContextTxInfo
      mResolvedDat <- case mDat of
        Nothing -> return Nothing
        Just (Api.Datum bDat) -> Just <$> fromBuiltinDataEither "datum" bDat
      return $ traceRunning "Spending" $ spendingTypedScript oRef mResolvedDat red txInfo
    Api.RewardingScript cred -> do
      (red, txInfo) <- deserializeContext "rewarding" scriptContextRedeemer scriptContextTxInfo
      return $ traceRunning "Rewarding" $ rewardingTypedScript cred red txInfo
    Api.CertifyingScript i cert -> do
      (red, txInfo) <- deserializeContext "certifying" scriptContextRedeemer scriptContextTxInfo
      return $ traceRunning "Certifying" $ certifyingTypedScript i cert red txInfo
    Api.VotingScript voter -> do
      (red, txInfo) <- deserializeContext "voting" scriptContextRedeemer scriptContextTxInfo
      return $ traceRunning "Voting" $ votingTypedScript voter red txInfo
    Api.ProposingScript i prop -> do
      (red, txInfo) <- deserializeContext "proposing" scriptContextRedeemer scriptContextTxInfo
      return $ traceRunning "Proposing" $ proposingTypedScript i prop red txInfo
  where
    fromBuiltinDataEither name = maybe (Left $ "Error when deserializing the " <> name) Right . fromBuiltinData
    traceRunning name = trace ("Running the validator with the " <> name <> " script purpose")
    deserializeContext name redData txInfoData = do
      red <- fromBuiltinDataEither (name <> " redeemer") redData
      txInfo <- fromBuiltinDataEither (name <> " tx info") txInfoData
      return (red, txInfo)

compileTypedMultiPurposeScript
  :: (TypedMultiPurposeScriptConstraints a) => TypedMultiPurposeScript a -> MultiPurposeScript
compileTypedMultiPurposeScript = compileUntypedMultiPurposeScript . typedToUntypedMultiPurposeScript

class ToBuiltinUnit a where
  toBuiltinUnit :: a -> BuiltinUnit

instance ToBuiltinUnit BuiltinUnit where
  toBuiltinUnit = id

instance ToBuiltinUnit () where
  toBuiltinUnit = BuiltinUnit

instance ToBuiltinUnit Bool where
  toBuiltinUnit = check

{-# INLINEABLE genericToUntypedMultiPurposeScript #-}
genericToUntypedMultiPurposeScript
  :: (FromData a, ToBuiltinUnit b) => (a -> b) -> UntypedMultiPurposeScript
genericToUntypedMultiPurposeScript script dat = case fromBuiltinData dat of
  Nothing -> traceError "Unable to deserialize to the desired type"
  Just ctx -> toBuiltinUnit $ script ctx

compileGenericMultiPurposeScript :: (FromData a, ToBuiltinUnit b) => (a -> b) -> MultiPurposeScript
compileGenericMultiPurposeScript = compileUntypedMultiPurposeScript . genericToUntypedMultiPurposeScript
