{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -g -fplugin-opt Plinth.Plugin:target-version=1.1.0 #-}

module Plutus.Script.Utils.V4.Generators
  ( trueCertifyingPurpose,
    trueGuardingPurpose,
    trueMintingPurpose,
    trueProposingPurpose,
    trueSpendingPurpose,
    trueVotingPurpose,
    trueWithdrawingPurpose,
    forwardingMintingPurpose,
    ownForwardingMintingPurpose,
    forwardingSpendingPurpose,
    ownForwardingSpendingPurpose,
    falseTypedMultiPurposeScript,
    trueMintingMPScript,
    trueSpendingMPScript,
    trueMPScript,
    falseMPScript,
  )
where

import Data.Maybe (Maybe (Just, Nothing))
import Plutus.Script.Utils.Scripts
  ( ToScript (toScript),
    ToScriptHash (toScriptHash),
  )
import Plutus.Script.Utils.V4.Typed
  ( CertifyingPurposeType,
    GuardingPurposeType,
    MintingPurposeType,
    MultiPurposeScript (MultiPurposeScript),
    ProposingPurposeType,
    SpendingPurposeType,
    TypedMultiPurposeScript
      ( TypedMultiPurposeScript,
        mintingPurpose,
        spendingPurpose
      ),
    VotingPurposeType,
    WithdrawingPurposeType,
    mkMultiPurposeScript,
  )
import PlutusLedgerApi.V4
  ( Address (Address),
    Credential (ScriptCredential),
    CurrencySymbol (CurrencySymbol),
    MintValue,
    ScriptHash (ScriptHash),
    TxInInfo (TxInInfo),
    TxOut (TxOut),
    TxOutRef,
    mintValueToMap,
  )
import PlutusTx.AssocMap qualified as Map
import PlutusTx.List (any, find)
import PlutusTx.Prelude
  ( Bool (False, True),
    BuiltinData,
    BuiltinUnit,
    Eq ((==)),
    check,
    ($),
    (.),
  )
import PlutusTx.TH (compile)

-- * Always true purposes

{-# INLINEABLE trueCertifyingPurpose #-}
trueCertifyingPurpose :: CertifyingPurposeType BuiltinData BuiltinData BuiltinData BuiltinData BuiltinData
trueCertifyingPurpose _ _ _ _ _ = True

{-# INLINEABLE trueGuardingPurpose #-}
trueGuardingPurpose :: GuardingPurposeType BuiltinData BuiltinData BuiltinData BuiltinData BuiltinData
trueGuardingPurpose _ _ _ _ _ = True

{-# INLINEABLE trueMintingPurpose #-}
trueMintingPurpose :: MintingPurposeType BuiltinData BuiltinData BuiltinData BuiltinData
trueMintingPurpose _ _ _ _ = True

{-# INLINEABLE trueProposingPurpose #-}
trueProposingPurpose :: ProposingPurposeType BuiltinData BuiltinData BuiltinData BuiltinData BuiltinData
trueProposingPurpose _ _ _ _ _ = True

{-# INLINEABLE trueSpendingPurpose #-}
trueSpendingPurpose :: SpendingPurposeType BuiltinData BuiltinData BuiltinData BuiltinData BuiltinData
trueSpendingPurpose _ _ _ _ _ = True

{-# INLINEABLE trueVotingPurpose #-}
trueVotingPurpose :: VotingPurposeType BuiltinData BuiltinData BuiltinData BuiltinData
trueVotingPurpose _ _ _ _ = True

{-# INLINEABLE trueWithdrawingPurpose #-}
trueWithdrawingPurpose :: WithdrawingPurposeType BuiltinData BuiltinData BuiltinData BuiltinData
trueWithdrawingPurpose _ _ _ _ = True

-- * Some additional purposes that can be used to build more complex scripts

-- | Minting script that ensures a given spending script is invoked in the transaction
{-# INLINEABLE forwardingMintingPurpose #-}
forwardingMintingPurpose ::
  (ToScriptHash script) =>
  script ->
  (mtx -> [TxInInfo]) ->
  MintingPurposeType ScriptHash CurrencySymbol red mtx
forwardingMintingPurpose (toScriptHash -> sHash) toTxInInfos _ _ _ (toTxInInfos -> txInInfos) =
  any (isScriptInputAt sHash) txInInfos

-- | Whether the given input is locked by the given script hash.
{-# INLINEABLE isScriptInputAt #-}
isScriptInputAt :: ScriptHash -> TxInInfo -> Bool
isScriptInputAt sHash (TxInInfo _ (TxOut (Address (ScriptCredential h) _) _ _ _)) = h == sHash
isScriptInputAt _ _ = False

-- | Minting policy that ensures the own spending script is invoked in the transaction
{-# INLINEABLE ownForwardingMintingPurpose #-}
ownForwardingMintingPurpose ::
  (mtx -> [TxInInfo]) ->
  MintingPurposeType ScriptHash CurrencySymbol red mtx
ownForwardingMintingPurpose toTxInInfos scriptHash cs = forwardingMintingPurpose cs toTxInInfos scriptHash cs

-- | Spending script that ensures a given minting script is invoked in the transaction
{-# INLINEABLE forwardingSpendingPurpose #-}
forwardingSpendingPurpose ::
  (ToScriptHash script) =>
  script ->
  (txInfo -> MintValue) ->
  SpendingPurposeType ScriptHash TxOutRef dat red txInfo
forwardingSpendingPurpose (toScriptHash -> ScriptHash hash) toMintValue _ _ _ _ (mintValueToMap . toMintValue -> mintValue) =
  CurrencySymbol hash `Map.member` mintValue

-- | Spending purpose that ensures the own minting purpose is invoked in the transaction
{-# INLINEABLE ownForwardingSpendingPurpose #-}
ownForwardingSpendingPurpose ::
  (txInfo -> MintValue) ->
  (txInfo -> [TxInInfo]) ->
  SpendingPurposeType ScriptHash TxOutRef dat red txInfo
ownForwardingSpendingPurpose toMintValue toTxInInfos scriptHash oRef dat red txInfo =
  case find (isInputAt oRef) (toTxInInfos txInfo) of
    Just (TxInInfo _ (TxOut (Address (ScriptCredential hash) _) _ _ _)) ->
      forwardingSpendingPurpose hash toMintValue scriptHash oRef dat red txInfo
    _ -> False

-- | Whether the given input consumes the given output reference.
{-# INLINEABLE isInputAt #-}
isInputAt :: TxOutRef -> TxInInfo -> Bool
isInputAt oRef (TxInInfo ref _) = ref == oRef

-- * Building multi purpose scripts

{-- Note on building multi-purpose scripts. They are usually built from the
  default-false template and configured purpose by purpose, rather than
  instantiated directly. Most scripts use only a few of the seven purposes. For
  example, to accept spending and reject the other purposes, use:

  @falseTypedMultiPurposeScript { spendingPurpose = Just trueSpendingPurpose }@
--}

-- | A typed multi-purpose script with no handlers enabled.
falseTypedMultiPurposeScript ::
  TypedMultiPurposeScript
    BuiltinData
    BuiltinData
    BuiltinData
    BuiltinData
    BuiltinData
    BuiltinData
    BuiltinData
    BuiltinData
    BuiltinData
    BuiltinData
    BuiltinData
    BuiltinData
    BuiltinData
    BuiltinData
    BuiltinData
    BuiltinData
    BuiltinData
    BuiltinData
    BuiltinData
    BuiltinData
    BuiltinData
    BuiltinData
    BuiltinData
    BuiltinData
    BuiltinData
    BuiltinData
    BuiltinData
    BuiltinData
    BuiltinData
    BuiltinData
    BuiltinData
    BuiltinData
falseTypedMultiPurposeScript =
  TypedMultiPurposeScript
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing

-- * Compiled scripts

{-- Note on creating and compiling multi-purpose scripts:

  1. Start from an existing template, such as @falseTypedMultiPurposeScript@

  2. Define the typed logics for the necessary purposes

  3. Assign these logics within the template

  4. Generate the script from the typed template

  5. Compile the script and wrap it in a @MultiPurposeScript@ envelope

  See examples below.
--}

-- | The multi-purpose script that accepts minting and rejects all other purposes.
trueMintingMPScript :: MultiPurposeScript
trueMintingMPScript = MultiPurposeScript $ toScript $$(compile [||script||])
  where
    script = mkMultiPurposeScript $ falseTypedMultiPurposeScript {mintingPurpose = Just trueMintingPurpose}

-- | The multi-purpose script that accepts spending and rejects all other purposes.
trueSpendingMPScript :: MultiPurposeScript
trueSpendingMPScript = MultiPurposeScript $ toScript $$(compile [||script||])
  where
    script = mkMultiPurposeScript $ falseTypedMultiPurposeScript {spendingPurpose = Just trueSpendingPurpose}

-- | The multi-purpose script that returns @True@ for all supported purposes.
trueMPScript :: MultiPurposeScript
trueMPScript = MultiPurposeScript $ toScript $$(compile [||script||])
  where
    script :: BuiltinData -> BuiltinUnit
    script _ = check True

falseMPScript :: MultiPurposeScript
falseMPScript = MultiPurposeScript $ toScript $$(compile [||script||])
  where
    script = mkMultiPurposeScript falseTypedMultiPurposeScript
