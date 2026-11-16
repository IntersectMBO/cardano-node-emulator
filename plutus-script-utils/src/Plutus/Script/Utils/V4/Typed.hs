{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Plutus.Script.Utils.V4.Typed
  ( CertifyingPurposeType,
    GuardingPurposeType,
    MintingPurposeType,
    ProposingPurposeType,
    SpendingPurposeType,
    TypedMultiPurposeScript (..),
    MultiPurposeScript (..),
    WithdrawingPurposeType,
    VotingPurposeType,
    mkMultiPurposeScript,
  )
where

import Codec.Serialise (Serialise)
import GHC.Generics (Generic)
import Plutus.Script.Utils.Scripts
  ( MintingPolicy (MintingPolicy),
    Script,
    StakeValidator (StakeValidator),
    ToMintingPolicy (toMintingPolicy),
    ToScript (toScript),
    ToStakeValidator (toStakeValidator),
    ToValidator (toValidator),
    ToVersioned (toVersioned),
    Validator (Validator),
    Versioned (Versioned),
  )
import PlutusLedgerApi.Common (PlutusLedgerLanguage (PlutusV4))
import PlutusLedgerApi.V4
  ( BuiltinData,
    FromData,
  )
import PlutusTx.Builtins (unsafeDataAsConstr)
import PlutusTx.Prelude
  ( Bool,
    BuiltinString,
    BuiltinUnit,
    Either (Left, Right),
    FromData (fromBuiltinData),
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
    (<>),
    (==),
  )
import Prelude qualified as H

-- * Various script purposes

{-- Note on types for the script purposes: Each purpose is associated with a
  function type with the following characteristics:

  - Each purpose returns a 'Bool' indicating whether validation succeeds.

  - Each purpose takes the hash of the script being validated and a redeemer
    of a purpose-specific type. The concrete redeemer type is chosen by the
    user, who can also customize how it is deserialized.

  - Each purpose takes a view of Plutus's @TxInfo@. This type is customizable
    (though it may simply be @TxInfo@), allowing users to deserialize only the
    fields they need.

  - Each purpose also takes parameters from @scriptContextScriptInfo@, which is
    decoded from the script context. For instance, a minting purpose receives
    its own currency symbol.

  - Spending takes an optional datum, and guarding takes the optional top-level
    transaction info, as additional user-defined parameters.
--}

-- | Certifying scripts take a script hash, an index, certificate, redeemer and txInfo
type CertifyingPurposeType scriptHash index cert red txInfo =
  scriptHash -> index -> cert -> red -> txInfo -> Bool

-- | Guarding scripts take a script hash, an index, optional top-level txInfo, redeemer and txInfo
type GuardingPurposeType scriptHash index topTxInfo red txInfo =
  scriptHash -> index -> Maybe topTxInfo -> red -> txInfo -> Bool

-- | Minting scripts take a script hash, their own currency symbol, redeemer and txInfo
type MintingPurposeType scriptHash currencySymbol red txInfo =
  scriptHash -> currencySymbol -> red -> txInfo -> Bool

-- | Proposing scripts take a script hash, an index, proposal, redeemer and txInfo
type ProposingPurposeType scriptHash index proposal red txInfo =
  scriptHash -> index -> proposal -> red -> txInfo -> Bool

-- | Spending scripts take a script hash, the utxo being consumed, an optional datum, redeemer and txInfo
type SpendingPurposeType scriptHash outRef dat red txInfo =
  scriptHash -> outRef -> Maybe dat -> red -> txInfo -> Bool

-- | Withdrawing scripts take a script hash, an account id, redeemer and txInfo
type WithdrawingPurposeType scriptHash accountId red txInfo =
  scriptHash -> accountId -> red -> txInfo -> Bool

-- | Voting scripts take a script hash, a voter, redeemer and txInfo
type VotingPurposeType scriptHash voter red txInfo = scriptHash -> voter -> red -> txInfo -> Bool

-- * Typed multi-purpose scripts

-- | Typed multi-purpose scripts provide the seven V4 purposes, each with their
-- own types as described above.
data
  TypedMultiPurposeScript
    certifyingScriptHash
    certifyingIndex
    certifyingCert
    certifyingRed
    certifyingTxInfo
    mintingScriptHash
    mintingCurrencySymbol
    mintingRed
    mintingTxInfo
    proposingScriptHash
    proposingIndex
    proposingProposal
    proposingRed
    proposingTxInfo
    withdrawingScriptHash
    withdrawingAccountId
    withdrawingRed
    withdrawingTxInfo
    spendingScriptHash
    spendingOutRef
    spendingDat
    spendingRed
    spendingTxInfo
    votingScriptHash
    votingVoter
    votingRed
    votingTxInfo
    guardingScriptHash
    guardingIndex
    guardingTopTxInfo
    guardingRed
    guardingTxInfo
  = ( FromData certifyingScriptHash,
      FromData certifyingIndex,
      FromData certifyingCert,
      FromData certifyingRed,
      FromData certifyingTxInfo,
      FromData mintingScriptHash,
      FromData mintingCurrencySymbol,
      FromData mintingRed,
      FromData mintingTxInfo,
      FromData proposingScriptHash,
      FromData proposingIndex,
      FromData proposingProposal,
      FromData proposingRed,
      FromData proposingTxInfo,
      FromData withdrawingScriptHash,
      FromData withdrawingAccountId,
      FromData withdrawingRed,
      FromData withdrawingTxInfo,
      FromData spendingScriptHash,
      FromData spendingOutRef,
      FromData spendingDat,
      FromData spendingRed,
      FromData spendingTxInfo,
      FromData votingScriptHash,
      FromData votingVoter,
      FromData votingRed,
      FromData votingTxInfo,
      FromData guardingScriptHash,
      FromData guardingIndex,
      FromData guardingTopTxInfo,
      FromData guardingRed,
      FromData guardingTxInfo
    ) =>
  TypedMultiPurposeScript
  { certifyingPurpose ::
      Maybe
        (CertifyingPurposeType certifyingScriptHash certifyingIndex certifyingCert certifyingRed certifyingTxInfo),
    mintingPurpose ::
      Maybe (MintingPurposeType mintingScriptHash mintingCurrencySymbol mintingRed mintingTxInfo),
    proposingPurpose ::
      Maybe
        (ProposingPurposeType proposingScriptHash proposingIndex proposingProposal proposingRed proposingTxInfo),
    withdrawingPurpose ::
      Maybe
        (WithdrawingPurposeType withdrawingScriptHash withdrawingAccountId withdrawingRed withdrawingTxInfo),
    spendingPurpose ::
      Maybe (SpendingPurposeType spendingScriptHash spendingOutRef spendingDat spendingRed spendingTxInfo),
    votingPurpose :: Maybe (VotingPurposeType votingScriptHash votingVoter votingRed votingTxInfo),
    guardingPurpose ::
      Maybe
        (GuardingPurposeType guardingScriptHash guardingIndex guardingTopTxInfo guardingRed guardingTxInfo)
  }

-- * Compiled multi-purpose scripts

-- | A 'MultiPurposeScript' wraps a Plutus V4 'Script'.
newtype MultiPurposeScript = MultiPurposeScript {getMultiPurposeScript :: Script}
  deriving stock (Generic)
  deriving newtype (H.Eq, H.Ord, Serialise)

instance ToScript MultiPurposeScript where
  toScript = getMultiPurposeScript

instance ToVersioned Script MultiPurposeScript where
  toVersioned = (`Versioned` PlutusV4) . toScript

instance ToValidator MultiPurposeScript where
  toValidator = Validator . toScript

instance ToVersioned Validator MultiPurposeScript where
  toVersioned = (`Versioned` PlutusV4) . toValidator

instance ToMintingPolicy MultiPurposeScript where
  toMintingPolicy = MintingPolicy . toScript

instance ToVersioned MintingPolicy MultiPurposeScript where
  toVersioned = (`Versioned` PlutusV4) . MintingPolicy . toScript

instance ToStakeValidator MultiPurposeScript where
  toStakeValidator = StakeValidator . toScript

instance ToVersioned StakeValidator MultiPurposeScript where
  toVersioned = (`Versioned` PlutusV4) . StakeValidator . toScript

-- * From 'TypedMultiPurposeScript' to 'MultiPurposeScript'

{-- Note on converting 'TypedMultiPurposeScript' to its on-chain representation:
  'TypedMultiPurposeScript' separates the purposes into manageable handlers.
  The on-chain script is a single function from 'BuiltinData' to 'BuiltinUnit'.
  To convert the typed representation, we:

  - Decode the outer script context and purpose information, leaving the
    @TxInfo@ and redeemer as 'BuiltinData' so each handler can deserialize them
    using its own types.

  - Use 'mkMultiPurposeScript' to build the on-chain script. It dispatches to
    the selected handler, decodes its typed arguments, and traces the selected
    purpose. Deserialization and unsupported-purpose errors are reported with
    'traceError'.
--}

-- | Script-context representation retaining the transaction info, redeemer,
-- script purpose arguments and script hash as raw data.
data ScriptContextResolvedScriptInfo
  = ScriptContextResolvedScriptInfo
      -- | the TxInfo
      BuiltinData
      -- | the Redeemer
      BuiltinData
      -- | the script purpose and its arguments
      (Integer, [BuiltinData])
      -- | the script hash
      BuiltinData

-- | Decode the outer context and purpose information while leaving the
-- transaction info, redeemer and script hash as raw data for the selected handler.
instance FromData ScriptContextResolvedScriptInfo where
  fromBuiltinData bData =
    case unsafeDataAsConstr bData of
      (_, [txInfo, redeemer, unsafeDataAsConstr -> (i, l), scriptHash]) ->
        Just $ ScriptContextResolvedScriptInfo txInfo redeemer (i, l) scriptHash
      _ -> Nothing

{-# INLINEABLE mkMultiPurposeScript #-}
mkMultiPurposeScript ::
  TypedMultiPurposeScript
    cScriptHash
    cIndex
    cCert
    cRed
    cTxInfo
    mScriptHash
    mCurrencySymbol
    mRed
    mTxInfo
    pScriptHash
    pIndex
    pProposal
    pRed
    pTxInfo
    wScriptHash
    wAccountId
    wRed
    wTxInfo
    sScriptHash
    sOutRef
    sDat
    sRed
    sTxInfo
    vScriptHash
    vVoter
    vRed
    vTxInfo
    gScriptHash
    gIndex
    gTopTxInfo
    gRed
    gTxInfo ->
  BuiltinData ->
  BuiltinUnit
mkMultiPurposeScript TypedMultiPurposeScript {..} dat =
  either traceError check $ do
    ScriptContextResolvedScriptInfo scTxInfo scRedeemer (purpose, args) scScriptHash <-
      fromBuiltinDataEither "script info" dat
    case args of
      [cur]
        | purpose == 0,
          Just mPurpose <- mintingPurpose -> do
            scriptHash <- fromBuiltinDataEither "script hash" scScriptHash
            currencySymbol <- fromBuiltinDataEither "minting currency symbol" cur
            red <- fromBuiltinDataEither "minting redeemer" scRedeemer
            txInfo <- fromBuiltinDataEither "minting tx info" scTxInfo
            return $ traceRunning "Minting" $ mPurpose scriptHash currencySymbol red txInfo
      [oRef, mDat]
        | purpose == 1,
          Just sPurpose <- spendingPurpose -> do
            scriptHash <- fromBuiltinDataEither "script hash" scScriptHash
            outRef <- fromBuiltinDataEither "spending output reference" oRef
            red <- fromBuiltinDataEither "spending redeemer" scRedeemer
            txInfo <- fromBuiltinDataEither "spending tx info" scTxInfo
            mResolvedDat <-
              let (constrIndex, fields) = unsafeDataAsConstr mDat
               in if constrIndex == 0
                    then case fields of
                      [bDat] -> Just <$> fromBuiltinDataEither "datum" bDat
                      _ -> Left "Error when deserializing the spending datum"
                    else return Nothing
            return $ traceRunning "Spending" $ sPurpose scriptHash outRef mResolvedDat red txInfo
      [accountId]
        | purpose == 2,
          Just wPurpose <- withdrawingPurpose -> do
            scriptHash <- fromBuiltinDataEither "script hash" scScriptHash
            resolvedAccountId <- fromBuiltinDataEither "withdrawing account id" accountId
            red <- fromBuiltinDataEither "withdrawing redeemer" scRedeemer
            txInfo <- fromBuiltinDataEither "withdrawing tx info" scTxInfo
            return $ traceRunning "Withdrawing" $ wPurpose scriptHash resolvedAccountId red txInfo
      [i, cert]
        | purpose == 3,
          Just cPurpose <- certifyingPurpose -> do
            scriptHash <- fromBuiltinDataEither "script hash" scScriptHash
            index <- fromBuiltinDataEither "certifying index" i
            certificate <- fromBuiltinDataEither "certifying certificate" cert
            red <- fromBuiltinDataEither "certifying redeemer" scRedeemer
            txInfo <- fromBuiltinDataEither "certifying tx info" scTxInfo
            return $ traceRunning "Certifying" $ cPurpose scriptHash index certificate red txInfo
      [voter]
        | purpose == 4,
          Just vPurpose <- votingPurpose -> do
            scriptHash <- fromBuiltinDataEither "script hash" scScriptHash
            resolvedVoter <- fromBuiltinDataEither "voting voter" voter
            red <- fromBuiltinDataEither "voting redeemer" scRedeemer
            txInfo <- fromBuiltinDataEither "voting tx info" scTxInfo
            return $ traceRunning "Voting" $ vPurpose scriptHash resolvedVoter red txInfo
      [i, prop]
        | purpose == 5,
          Just pPurpose <- proposingPurpose -> do
            scriptHash <- fromBuiltinDataEither "script hash" scScriptHash
            index <- fromBuiltinDataEither "proposing index" i
            proposal <- fromBuiltinDataEither "proposing proposal" prop
            red <- fromBuiltinDataEither "proposing redeemer" scRedeemer
            txInfo <- fromBuiltinDataEither "proposing tx info" scTxInfo
            return $ traceRunning "Proposing" $ pPurpose scriptHash index proposal red txInfo
      [i, mTopTxInfo]
        | purpose == 6,
          Just gPurpose <- guardingPurpose -> do
            scriptHash <- fromBuiltinDataEither "script hash" scScriptHash
            index <- fromBuiltinDataEither "guarding index" i
            topTxInfo <- fromBuiltinDataEither "guarding top-level tx info" mTopTxInfo
            red <- fromBuiltinDataEither "guarding redeemer" scRedeemer
            txInfo <- fromBuiltinDataEither "guarding tx info" scTxInfo
            return $ traceRunning "Guarding" $ gPurpose scriptHash index topTxInfo red txInfo
      _ -> traceError "Script used with an unsupported purpose."
  where
    fromBuiltinDataEither :: (FromData a) => BuiltinString -> BuiltinData -> Either BuiltinString a
    fromBuiltinDataEither name = maybe (Left $ "Error when deserializing the " <> name) Right . fromBuiltinData

    traceRunning :: BuiltinString -> Bool -> Bool
    traceRunning name = trace ("Running the validator with the " <> name <> " script purpose")
