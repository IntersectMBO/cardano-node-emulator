{-# LANGUAGE NoImplicitPrelude #-}

module Plutus.Script.Utils.V3.Typed
  ( CertifyingPurposeType,
    MintingPurposeType,
    ProposingPurposeType,
    RewardingPurposeType,
    SpendingPurposeType,
    VotingPurposeType,
    TypedMultiPurposeScript (..),
    MultiPurposeScript (..),
    mkMultiPurposeScript,
  )
where

import Codec.Serialise (Serialise)
import GHC.Generics (Generic)
import Plutus.Script.Utils.Address
  ( ToAddress (toAddress),
    ToCredential (toCredential),
  )
import Plutus.Script.Utils.Scripts
  ( Language (PlutusV3),
    MintingPolicy (MintingPolicy),
    Script,
    StakeValidator (StakeValidator),
    ToMintingPolicy (toMintingPolicy),
    ToMintingPolicyHash (toMintingPolicyHash),
    ToScript (toScript),
    ToScriptHash (toScriptHash),
    ToStakeValidator (toStakeValidator),
    ToStakeValidatorHash (toStakeValidatorHash),
    ToValidator (toValidator),
    ToValidatorHash (toValidatorHash),
    ToVersioned (toVersioned),
    Validator (Validator),
    Versioned (Versioned),
  )
import PlutusLedgerApi.V3
  ( Address (Address),
    BuiltinData,
    Credential (ScriptCredential),
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

  - Each purpose takes a redeemer of a purpose-specific type. The concrete
    redeemer type is chosen by the user, who can also customize how it is
    deserialized.

  - Each purpose takes a view of Plutus's @TxInfo@. This type is customizable
    (though it may simply be @TxInfo@), allowing users to deserialize only the
    fields they need. Scripts rarely need every field of @TxInfo@, so a custom
    type is often preferable.

  - Each purpose also takes parameters from @scriptContextScriptInfo@, which is
    decoded from the script context. For instance, a minting purpose receives
    its own currency symbol.

  - The spending purpose takes an optional datum as an additional parameter.
    Like the redeemer and transaction-info view, its type is user-defined.
--}

-- | Certifying scripts take an index, certificate, redeemer and txInfo
type CertifyingPurposeType index cert red txInfo = index -> cert -> red -> txInfo -> Bool

-- | Minting scripts take their own currency symbol, redeemer and txInfo
type MintingPurposeType currencySymbol red txInfo = currencySymbol -> red -> txInfo -> Bool

-- | Proposing scripts take an index, proposal, redeemer and txInfo
type ProposingPurposeType index proposal red txInfo = index -> proposal -> red -> txInfo -> Bool

-- | Rewarding scripts take a credential, redeemer and txInfo
type RewardingPurposeType credential red txInfo = credential -> red -> txInfo -> Bool

-- | Spending scripts take the utxo being consumed, an optional datum, redeemer and txInfo
type SpendingPurposeType outRef dat red txInfo = outRef -> Maybe dat -> red -> txInfo -> Bool

-- | Voting scripts take a voter, redeemer and txInfo
type VotingPurposeType voter red txInfo = voter -> red -> txInfo -> Bool

-- * Typed multi-purpose scripts

-- | Typed multi-purpose scripts provide 6 different purposes, each with their
-- own types as described above.
data
  TypedMultiPurposeScript
    certifyingIndex
    certifyingCert
    certifyingRed
    certifyingTxInfo
    mintingCurrencySymbol
    mintingRed
    mintingTxInfo
    proposingIndex
    proposingProposal
    proposingRed
    proposingTxInfo
    rewardingCredential
    rewardingRed
    rewardingTxInfo
    spendingOutRef
    spendingDat
    spendingRed
    spendingTxInfo
    votingVoter
    votingRed
    votingTxInfo
  = ( FromData certifyingIndex,
      FromData certifyingCert,
      FromData certifyingRed,
      FromData certifyingTxInfo,
      FromData mintingCurrencySymbol,
      FromData mintingRed,
      FromData mintingTxInfo,
      FromData proposingIndex,
      FromData proposingProposal,
      FromData proposingRed,
      FromData proposingTxInfo,
      FromData rewardingCredential,
      FromData rewardingRed,
      FromData rewardingTxInfo,
      FromData spendingOutRef,
      FromData spendingDat,
      FromData spendingRed,
      FromData spendingTxInfo,
      FromData votingVoter,
      FromData votingRed,
      FromData votingTxInfo
    ) =>
  TypedMultiPurposeScript
  { certifyingPurpose :: Maybe (CertifyingPurposeType certifyingIndex certifyingCert certifyingRed certifyingTxInfo),
    mintingPurpose :: Maybe (MintingPurposeType mintingCurrencySymbol mintingRed mintingTxInfo),
    proposingPurpose :: Maybe (ProposingPurposeType proposingIndex proposingProposal proposingRed proposingTxInfo),
    rewardingPurpose :: Maybe (RewardingPurposeType rewardingCredential rewardingRed rewardingTxInfo),
    spendingPurpose :: Maybe (SpendingPurposeType spendingOutRef spendingDat spendingRed spendingTxInfo),
    votingPurpose :: Maybe (VotingPurposeType votingVoter votingRed votingTxInfo)
  }

-- * Compiled multi-purpose scripts

-- | A 'MultiPurposeScript' wraps a Plutus V3 'Script'.
newtype MultiPurposeScript = MultiPurposeScript {getMultiPurposeScript :: Script}
  deriving stock (Generic)
  deriving newtype (H.Eq, H.Ord, Serialise)

instance H.Show MultiPurposeScript where
  show = ("Multi-purpose script #" <>) . H.show . toScriptHash

instance ToScript MultiPurposeScript where
  toScript = getMultiPurposeScript

instance ToVersioned Script MultiPurposeScript where
  toVersioned = (`Versioned` PlutusV3) . toScript

instance ToScriptHash MultiPurposeScript where
  toScriptHash = toScriptHash . toVersioned @Script

instance ToCredential MultiPurposeScript where
  toCredential = ScriptCredential . toScriptHash

instance ToAddress MultiPurposeScript where
  toAddress = (`Address` Nothing) . toCredential

instance ToValidator MultiPurposeScript where
  toValidator = Validator . toScript

instance ToVersioned Validator MultiPurposeScript where
  toVersioned = (`Versioned` PlutusV3) . toValidator

instance ToValidatorHash MultiPurposeScript where
  toValidatorHash = toValidatorHash . toScriptHash

instance ToMintingPolicy MultiPurposeScript where
  toMintingPolicy = MintingPolicy . toScript

instance ToVersioned MintingPolicy MultiPurposeScript where
  toVersioned = (`Versioned` PlutusV3) . MintingPolicy . toScript

instance ToMintingPolicyHash MultiPurposeScript where
  toMintingPolicyHash = toMintingPolicyHash . toScriptHash

instance ToStakeValidator MultiPurposeScript where
  toStakeValidator = StakeValidator . toScript

instance ToVersioned StakeValidator MultiPurposeScript where
  toVersioned = (`Versioned` PlutusV3) . StakeValidator . toScript

instance ToStakeValidatorHash MultiPurposeScript where
  toStakeValidatorHash = toStakeValidatorHash . toScriptHash

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

-- | Script-context representation retaining the transaction info and redeemer
-- as raw data while decoding the purpose tag and arguments.
data ScriptContextResolvedScriptInfo
  = ScriptContextResolvedScriptInfo
      -- | the TxInfo
      BuiltinData
      -- | the Redeemer
      BuiltinData
      -- | the script purpose and its arguments
      (Integer, [BuiltinData])

-- | Decode the outer context and purpose information while leaving the
-- transaction info and redeemer as raw data for the selected handler.
instance FromData ScriptContextResolvedScriptInfo where
  fromBuiltinData bData =
    case unsafeDataAsConstr bData of
      (_, [txInfo, redeemer, unsafeDataAsConstr -> (i, l)]) ->
        Just $ ScriptContextResolvedScriptInfo txInfo redeemer (i, l)
      _ -> Nothing

{-# INLINEABLE mkMultiPurposeScript #-}
mkMultiPurposeScript ::
  TypedMultiPurposeScript
    cIndex
    cCert
    cRed
    cTxInfo
    mCurrencySymbol
    mRed
    mTxInfo
    pIndex
    pProposal
    pRed
    pTxInfo
    rCredential
    rRed
    rTxInfo
    sOutRef
    sDat
    sRed
    sTxInfo
    vVoter
    vRed
    vTxInfo ->
  BuiltinData ->
  BuiltinUnit
mkMultiPurposeScript TypedMultiPurposeScript {..} dat =
  either traceError check $ do
    ScriptContextResolvedScriptInfo scTxInfo scRedeemer (purpose, args) <- fromBuiltinDataEither "script info" dat
    case args of
      [i, cert]
        | purpose == 0,
          Just cPurpose <- certifyingPurpose -> do
            index <- fromBuiltinDataEither "certifying index" i
            certificate <- fromBuiltinDataEither "certifying certificate" cert
            red <- fromBuiltinDataEither "certifying redeemer" scRedeemer
            txInfo <- fromBuiltinDataEither "certifying tx info" scTxInfo
            return $ traceRunning "Certifying" $ cPurpose index certificate red txInfo
      [cur]
        | purpose == 1,
          Just mPurpose <- mintingPurpose -> do
            currencySymbol <- fromBuiltinDataEither "minting currency symbol" cur
            red <- fromBuiltinDataEither "minting redeemer" scRedeemer
            txInfo <- fromBuiltinDataEither "minting tx info" scTxInfo
            return $ traceRunning "Minting" $ mPurpose currencySymbol red txInfo
      [i, prop]
        | purpose == 2,
          Just pPurpose <- proposingPurpose -> do
            index <- fromBuiltinDataEither "proposing index" i
            proposal <- fromBuiltinDataEither "proposing proposal" prop
            red <- fromBuiltinDataEither "proposing redeemer" scRedeemer
            txInfo <- fromBuiltinDataEither "proposing tx info" scTxInfo
            return $ traceRunning "Proposing" $ pPurpose index proposal red txInfo
      [cred]
        | purpose == 3,
          Just rPurpose <- rewardingPurpose -> do
            credential <- fromBuiltinDataEither "rewarding credential" cred
            red <- fromBuiltinDataEither "rewarding redeemer" scRedeemer
            txInfo <- fromBuiltinDataEither "rewarding tx info" scTxInfo
            return $ traceRunning "Rewarding" $ rPurpose credential red txInfo
      [oRef, mDat]
        | purpose == 4,
          Just sPurpose <- spendingPurpose -> do
            outRef <- fromBuiltinDataEither "spending output reference" oRef
            red <- fromBuiltinDataEither "spending redeemer" scRedeemer
            txInfo <- fromBuiltinDataEither "spending tx info" scTxInfo
            mResolvedDat <- case unsafeDataAsConstr mDat of
              (_, [unsafeDataAsConstr -> (_, [bDat])]) -> Just <$> fromBuiltinDataEither "datum" bDat
              _ -> return Nothing
            return $ traceRunning "Spending" $ sPurpose outRef mResolvedDat red txInfo
      [voter]
        | purpose == 5,
          Just vPurpose <- votingPurpose -> do
            resolvedVoter <- fromBuiltinDataEither "voting voter" voter
            red <- fromBuiltinDataEither "voting redeemer" scRedeemer
            txInfo <- fromBuiltinDataEither "voting tx info" scTxInfo
            return $ traceRunning "Voting" $ vPurpose resolvedVoter red txInfo
      _ -> traceError "Script used with an unsupported purpose."
  where
    fromBuiltinDataEither :: (FromData a) => BuiltinString -> BuiltinData -> Either BuiltinString a
    fromBuiltinDataEither name = maybe (Left $ "Error when deserializing the " <> name) Right . fromBuiltinData

    traceRunning :: BuiltinString -> Bool -> Bool
    traceRunning name = trace ("Running the validator with the " <> name <> " script purpose")
