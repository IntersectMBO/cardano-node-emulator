{-# LANGUAGE NoImplicitPrelude #-}

module Plutus.Script.Utils.V4.Generators
  ( trueCertifyingPurpose,
    trueMintingPurpose,
    forwardingMintingPurpose,
    ownForwardingMintingPurpose,
    trueProposingPurpose,
    trueWithdrawingPurpose,
    trueSpendingPurpose,
    forwardingSpendingPurpose,
    ownForwardingSpendingPurpose,
    trueVotingPurpose,
    trueGuardingPurpose,
    falseTypedMultiPurposeScript,
    trueTypedMultiPurposeScript,
    withCertifyingPurpose,
    addCertifyingPurpose,
    withMintingPurpose,
    addMintingPurpose,
    withProposingPurpose,
    addProposingPurpose,
    withWithdrawingPurpose,
    addWithdrawingPurpose,
    withSpendingPurpose,
    addSpendingPurpose,
    withVotingPurpose,
    addVotingPurpose,
    withGuardingPurpose,
    addGuardingPurpose,
    trueMintingMPScript,
    trueSpendingMPScript,
    falseMPScript,
    trueMPScript,
    multiPurposeScriptValue,
  )
where

import Data.Maybe (Maybe (Just, Nothing), fromMaybe)
import Plutus.Script.Utils.Scripts
  ( ScriptHash (ScriptHash),
    ToScript (toScript),
    ToScriptHash (toScriptHash),
    toCurrencySymbol,
  )
import Plutus.Script.Utils.V4.Typed
  ( CertifyingPurposeType',
    GuardingPurposeType',
    MintingPurposeType',
    MultiPurposeScript (MultiPurposeScript),
    ProposingPurposeType',
    SpendingPurposeType',
    TypedMultiPurposeScript
      ( TypedMultiPurposeScript,
        certifyingPurpose,
        guardingPurpose,
        mintingPurpose,
        proposingPurpose,
        spendingPurpose,
        votingPurpose,
        withdrawingPurpose
      ),
    VotingPurposeType',
    WithdrawingPurposeType',
    mkMultiPurposeScript,
  )
import PlutusLedgerApi.V4
  ( Credential (ScriptCredential),
    CurrencySymbol (CurrencySymbol),
    FromData,
    MintValue,
    TokenName,
    TxInInfo (TxInInfo),
    TxOut (TxOut),
    Value,
    mintValueToMap,
    singleton,
  )
import PlutusLedgerApi.V4.Address (Address (Address))
import PlutusTx.AssocMap qualified as Map
import PlutusTx.List (elem)
import PlutusTx.Prelude
  ( Bool (False, True),
    Eq ((==)),
    Integer,
    ($),
    (&&),
    (.),
  )
import PlutusTx.TH (compile)

trueCertifyingPurpose :: CertifyingPurposeType' red txInfo
trueCertifyingPurpose _ _ _ _ = True

trueMintingPurpose :: MintingPurposeType' red txInfo
trueMintingPurpose _ _ _ = True

-- | Minting script that ensures a given spending script is invoked in the transaction
{-# INLINEABLE forwardingMintingPurpose #-}
forwardingMintingPurpose :: (ToScriptHash script) => script -> (mtx -> [TxInInfo]) -> MintingPurposeType' red mtx
forwardingMintingPurpose (toScriptHash -> sHash) toTxInInfos _ _ (toTxInInfos -> txInInfos) =
  sHash `elem` [h | TxInInfo _ (TxOut (Address (ScriptCredential h) _) _ _ _) <- txInInfos]

-- | Minting policy that ensures the own spending script is invoked in the transaction
{-# INLINEABLE ownForwardingMintingPurpose #-}
ownForwardingMintingPurpose :: (mtx -> [TxInInfo]) -> MintingPurposeType' red mtx
ownForwardingMintingPurpose toTxInInfos cs = forwardingMintingPurpose cs toTxInInfos cs

trueProposingPurpose :: ProposingPurposeType' red txInfo
trueProposingPurpose _ _ _ _ = True

trueWithdrawingPurpose :: WithdrawingPurposeType' red txInfo
trueWithdrawingPurpose _ _ _ = True

trueSpendingPurpose :: SpendingPurposeType' dat red txInfo
trueSpendingPurpose _ _ _ _ = True

-- | Spending script that ensures a given minting script is invoked in the transaction
{-# INLINEABLE forwardingSpendingPurpose #-}
forwardingSpendingPurpose :: (ToScriptHash script) => script -> (txInfo -> MintValue) -> SpendingPurposeType' dat red txInfo
forwardingSpendingPurpose (toScriptHash -> ScriptHash hash) toMintValue _ _ _ (mintValueToMap . toMintValue -> mintValue) =
  CurrencySymbol hash `Map.member` mintValue

-- | Spending purpose that ensures the own minting purpose is invoked in the transaction
{-# INLINEABLE ownForwardingSpendingPurpose #-}
ownForwardingSpendingPurpose :: (txInfo -> MintValue) -> (txInfo -> [TxInInfo]) -> SpendingPurposeType' dat red txInfo
ownForwardingSpendingPurpose toMintValue toTxInInfos oRef dat red txInfo =
  case [hash | TxInInfo ref (TxOut (Address (ScriptCredential hash) _) _ _ _) <- toTxInInfos txInfo, ref == oRef] of
    [hash] -> forwardingSpendingPurpose hash toMintValue oRef dat red txInfo
    _ -> False

trueVotingPurpose :: VotingPurposeType' red txInfo
trueVotingPurpose _ _ _ = True

trueGuardingPurpose :: GuardingPurposeType' red txInfo
trueGuardingPurpose _ _ _ _ = True

-- * Building multi purpose scripts

{-- Note on building multi-purpose scripts. Multi-purpose scripts are never meant
  to be instantiated directly. The rationale is that most script will seldom be
  used for more than 2 purposes. Thus it is more convient to start from a
  default instance and build up the script from there. To that end, we provide
  three facilities:

  - 2 default instances, one that say No for every purpose, and one that says
    Yes for every purpose. Most likely, the former will be the right starting
    point in almost every cases.

  - Helpers @withXXXpurpose@ to override a given purpose

  - Helpers @addXXXpurpose@ to add constraints to an existing purpose. While
    this can be detrimental to use this when the parameters need to be
    deserialized in several constraints, it can prove useful when working with
    several unrelated constraints. Using this is left at the user's discretion.

  Example: if you want a script that fails for all purposes but spending and
  always succeeds otherwise, use:
  @falseTypedMultiPurposeScript `withSpendingPurpose` trueSpendingPurpose@

--}

falseTypedMultiPurposeScript :: TypedMultiPurposeScript () () () () () () () () () () () () () () ()
falseTypedMultiPurposeScript =
  TypedMultiPurposeScript
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing

trueTypedMultiPurposeScript :: TypedMultiPurposeScript () () () () () () () () () () () () () () ()
trueTypedMultiPurposeScript =
  TypedMultiPurposeScript
    (Just trueCertifyingPurpose)
    (Just trueMintingPurpose)
    (Just trueProposingPurpose)
    (Just trueWithdrawingPurpose)
    (Just trueSpendingPurpose)
    (Just trueVotingPurpose)
    (Just trueGuardingPurpose)

-- | Overrides the certifying purpose
withCertifyingPurpose ::
  (FromData cr', FromData ctx') =>
  TypedMultiPurposeScript cr ctx mr mtx pr ptx wr wtx sd sr stx vr vtx gr gtx ->
  CertifyingPurposeType' cr' ctx' ->
  TypedMultiPurposeScript cr' ctx' mr mtx pr ptx wr wtx sd sr stx vr vtx gr gtx
withCertifyingPurpose ts cs = ts {certifyingPurpose = Just cs}

-- | Combines a new certifying purpose with the existing one
addCertifyingPurpose ::
  TypedMultiPurposeScript cr ctx mr mtx pr ptx wr wtx sd sr stx vr vtx gr gtx ->
  CertifyingPurposeType' cr ctx ->
  TypedMultiPurposeScript cr ctx mr mtx pr ptx wr wtx sd sr stx vr vtx gr gtx
addCertifyingPurpose ts@(TypedMultiPurposeScript {certifyingPurpose}) cs =
  ts `withCertifyingPurpose` \ix cert red txInfo ->
    fromMaybe trueCertifyingPurpose certifyingPurpose ix cert red txInfo
      && cs ix cert red txInfo

-- | Overrides the certifying purpose
withMintingPurpose ::
  (FromData mr', FromData mtx') =>
  TypedMultiPurposeScript cr ctx mr mtx pr ptx wr wtx sd sr stx vr vtx gr gtx ->
  MintingPurposeType' mr' mtx' ->
  TypedMultiPurposeScript cr ctx mr' mtx' pr ptx wr wtx sd sr stx vr vtx gr gtx
withMintingPurpose ts ms = ts {mintingPurpose = Just ms}

-- | Combines a new minting purpose with the existing one
addMintingPurpose ::
  TypedMultiPurposeScript cr ctx mr mtx pr ptx wr wtx sd sr stx vr vtx gr gtx ->
  MintingPurposeType' mr mtx ->
  TypedMultiPurposeScript cr ctx mr mtx pr ptx wr wtx sd sr stx vr vtx gr gtx
addMintingPurpose ts@(TypedMultiPurposeScript {mintingPurpose}) ms =
  ts `withMintingPurpose` \cs red txInfo ->
    fromMaybe trueMintingPurpose mintingPurpose cs red txInfo
      && ms cs red txInfo

-- | Overrides the proposing purpose
withProposingPurpose ::
  (FromData pr', FromData ptx') =>
  TypedMultiPurposeScript cr ctx mr mtx pr ptx wr wtx sd sr stx vr vtx gr gtx ->
  ProposingPurposeType' pr' ptx' ->
  TypedMultiPurposeScript cr ctx mr mtx pr' ptx' wr wtx sd sr stx vr vtx gr gtx
withProposingPurpose ts ps = ts {proposingPurpose = Just ps}

-- | Combines a new proposing purpose with the existing one
addProposingPurpose ::
  TypedMultiPurposeScript cr ctx mr mtx pr ptx wr wtx sd sr stx vr vtx gr gtx ->
  ProposingPurposeType' pr ptx ->
  TypedMultiPurposeScript cr ctx mr mtx pr ptx wr wtx sd sr stx vr vtx gr gtx
addProposingPurpose ts@(TypedMultiPurposeScript {proposingPurpose}) ps =
  ts `withProposingPurpose` \ix prop red txInfo ->
    fromMaybe trueProposingPurpose proposingPurpose ix prop red txInfo
      && ps ix prop red txInfo

-- | Overrides the withdrawing purpose
withWithdrawingPurpose ::
  (FromData wr', FromData wtx') =>
  TypedMultiPurposeScript cr ctx mr mtx pr ptx wr wtx sd sr stx vr vtx gr gtx ->
  WithdrawingPurposeType' wr' wtx' ->
  TypedMultiPurposeScript cr ctx mr mtx pr ptx wr' wtx' sd sr stx vr vtx gr gtx
withWithdrawingPurpose ts ws = ts {withdrawingPurpose = Just ws}

-- | Combines a new withdrawing purpose with the existing one
addWithdrawingPurpose ::
  TypedMultiPurposeScript cr ctx mr mtx pr ptx wr wtx sd sr stx vr vtx gr gtx ->
  WithdrawingPurposeType' wr wtx ->
  TypedMultiPurposeScript cr ctx mr mtx pr ptx wr wtx sd sr stx vr vtx gr gtx
addWithdrawingPurpose ts@(TypedMultiPurposeScript {withdrawingPurpose}) ws =
  ts `withWithdrawingPurpose` \accId red txInfo ->
    fromMaybe trueWithdrawingPurpose withdrawingPurpose accId red txInfo
      && ws accId red txInfo

-- | Overrides the spending purpose
withSpendingPurpose ::
  (FromData sd', FromData sr', FromData stx') =>
  TypedMultiPurposeScript cr ctx mr mtx pr ptx wr wtx sd sr stx vr vtx gr gtx ->
  SpendingPurposeType' sd' sr' stx' ->
  TypedMultiPurposeScript cr ctx mr mtx pr ptx wr wtx sd' sr' stx' vr vtx gr gtx
withSpendingPurpose ts ss = ts {spendingPurpose = Just ss}

-- | Combines a new spending purpose with the existing one
addSpendingPurpose ::
  TypedMultiPurposeScript cr ctx mr mtx pr ptx wr wtx sd sr stx vr vtx gr gtx ->
  SpendingPurposeType' sd sr stx ->
  TypedMultiPurposeScript cr ctx mr mtx pr ptx wr wtx sd sr stx vr vtx gr gtx
addSpendingPurpose ts@(TypedMultiPurposeScript {spendingPurpose}) ss =
  ts `withSpendingPurpose` \oRef mDat red txInfo ->
    fromMaybe trueSpendingPurpose spendingPurpose oRef mDat red txInfo
      && ss oRef mDat red txInfo

-- | Overrides the voting purpose
withVotingPurpose ::
  (FromData vr', FromData vtx') =>
  TypedMultiPurposeScript cr ctx mr mtx pr ptx wr wtx sd sr stx vr vtx gr gtx ->
  VotingPurposeType' vr' vtx' ->
  TypedMultiPurposeScript cr ctx mr mtx pr ptx wr wtx sd sr stx vr' vtx' gr gtx
withVotingPurpose ts vs = ts {votingPurpose = Just vs}

-- | Combines a new voting purpose with the existing one
addVotingPurpose ::
  TypedMultiPurposeScript cr ctx mr mtx pr ptx wr wtx sd sr stx vr vtx gr gtx ->
  VotingPurposeType' vr vtx ->
  TypedMultiPurposeScript cr ctx mr mtx pr ptx wr wtx sd sr stx vr vtx gr gtx
addVotingPurpose ts@(TypedMultiPurposeScript {votingPurpose}) vs =
  ts `withVotingPurpose` \voter red txInfo ->
    fromMaybe trueVotingPurpose votingPurpose voter red txInfo
      && vs voter red txInfo

-- | Overrides the guarding purpose
withGuardingPurpose ::
  (FromData gr', FromData gtx') =>
  TypedMultiPurposeScript cr ctx mr mtx pr ptx wr wtx sd sr stx vr vtx gr gtx ->
  GuardingPurposeType' gr' gtx' ->
  TypedMultiPurposeScript cr ctx mr mtx pr ptx wr wtx sd sr stx vr vtx gr' gtx'
withGuardingPurpose ts gs = ts {guardingPurpose = Just gs}

-- | Combines a new guarding purpose with the existing one
addGuardingPurpose ::
  TypedMultiPurposeScript cr ctx mr mtx pr ptx wr wtx sd sr stx vr vtx gr gtx ->
  GuardingPurposeType' gr gtx ->
  TypedMultiPurposeScript cr ctx mr mtx pr ptx wr wtx sd sr stx vr vtx gr gtx
addGuardingPurpose ts@(TypedMultiPurposeScript {guardingPurpose}) gs =
  ts `withGuardingPurpose` \ix mTopTxInfo red txInfo ->
    fromMaybe trueGuardingPurpose guardingPurpose ix mTopTxInfo red txInfo
      && gs ix mTopTxInfo red txInfo

-- * Creating and compiling multi-purpose scripts

{-- Note on creating and compiling multi-purpose scripts: We provide two examples
  below to showcase how one can go from their abstract script reprentation to a
  compiled plutus scripts. The steps are as follows:

  1. Start from an existing template, such as @falseTypedMultiPurposeScript@

  2. Define the typed logics for the necessary purposes

  3. Assign these logics within the template

  4. Generate the script from the typed template

  5. Compile the script and wrap it into a @MultiPurposeScript@ enveloppe

--}

-- | The multi-purpose script that returns @True@ in minting purpose and @False@
-- otherwise
trueMintingMPScript :: MultiPurposeScript a
trueMintingMPScript = MultiPurposeScript $ toScript $$(compile [||script||])
  where
    script =
      mkMultiPurposeScript
        $ falseTypedMultiPurposeScript
        `withMintingPurpose` (trueMintingPurpose @() @())

-- | The multi-purpose script that returns @True@ in spending purpose and @False@
-- otherwise
trueSpendingMPScript :: MultiPurposeScript a
trueSpendingMPScript = MultiPurposeScript $ toScript $$(compile [||script||])
  where
    script =
      mkMultiPurposeScript
        $ falseTypedMultiPurposeScript
        `withSpendingPurpose` trueSpendingPurpose @() @() @()

-- | The multi-purpose script that returns @True@ in all purposes
trueMPScript :: MultiPurposeScript a
trueMPScript = MultiPurposeScript $ toScript $$(compile [||script||])
  where
    script = mkMultiPurposeScript trueTypedMultiPurposeScript

falseMPScript :: MultiPurposeScript a
falseMPScript = MultiPurposeScript $ toScript $$(compile [||script||])
  where
    script = mkMultiPurposeScript falseTypedMultiPurposeScript

multiPurposeScriptValue :: MultiPurposeScript a -> TokenName -> Integer -> Value
multiPurposeScriptValue mpScript = singleton (toCurrencySymbol mpScript)
