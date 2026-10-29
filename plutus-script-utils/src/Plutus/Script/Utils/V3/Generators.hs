{-# LANGUAGE NoImplicitPrelude #-}

module Plutus.Script.Utils.V3.Generators
  ( trueCertifyingPurpose,
    trueMintingPurpose,
    trueProposingPurpose,
    trueRewardingPurpose,
    trueSpendingPurpose,
    trueVotingPurpose,
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
  ( ScriptHash (ScriptHash),
    ToScript (toScript),
    ToScriptHash (toScriptHash),
  )
import Plutus.Script.Utils.V3.Typed
  ( CertifyingPurposeType,
    MintingPurposeType,
    MultiPurposeScript (MultiPurposeScript),
    ProposingPurposeType,
    RewardingPurposeType,
    SpendingPurposeType,
    TypedMultiPurposeScript
      ( TypedMultiPurposeScript,
        mintingPurpose,
        spendingPurpose
      ),
    VotingPurposeType,
    mkMultiPurposeScript,
  )
import PlutusLedgerApi.V1.Address (Address (Address))
import PlutusLedgerApi.V3
  ( Credential (ScriptCredential),
    CurrencySymbol (CurrencySymbol),
    MintValue,
    TxInInfo (TxInInfo),
    TxOut (TxOut),
    TxOutRef,
    mintValueToMap,
  )
import PlutusTx.AssocMap qualified as Map
import PlutusTx.List (elem)
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
trueCertifyingPurpose :: CertifyingPurposeType BuiltinData BuiltinData BuiltinData BuiltinData
trueCertifyingPurpose _ _ _ _ = True

{-# INLINEABLE trueMintingPurpose #-}
trueMintingPurpose :: MintingPurposeType BuiltinData BuiltinData BuiltinData
trueMintingPurpose _ _ _ = True

{-# INLINEABLE trueProposingPurpose #-}
trueProposingPurpose :: ProposingPurposeType BuiltinData BuiltinData BuiltinData BuiltinData
trueProposingPurpose _ _ _ _ = True

{-# INLINEABLE trueRewardingPurpose #-}
trueRewardingPurpose :: RewardingPurposeType BuiltinData BuiltinData BuiltinData
trueRewardingPurpose _ _ _ = True

{-# INLINEABLE trueSpendingPurpose #-}
trueSpendingPurpose :: SpendingPurposeType BuiltinData BuiltinData BuiltinData BuiltinData
trueSpendingPurpose _ _ _ _ = True

{-# INLINEABLE trueVotingPurpose #-}
trueVotingPurpose :: VotingPurposeType BuiltinData BuiltinData BuiltinData
trueVotingPurpose _ _ _ = True

-- * Some additional purposes that can be used to build more complex scripts

-- | Minting script that ensures a given spending script is invoked in the transaction
{-# INLINEABLE forwardingMintingPurpose #-}
forwardingMintingPurpose :: (ToScriptHash script) => script -> (mtx -> [TxInInfo]) -> MintingPurposeType CurrencySymbol red mtx
forwardingMintingPurpose (toScriptHash -> sHash) toTxInInfos _ _ (toTxInInfos -> txInInfos) =
  sHash `elem` [h | TxInInfo _ (TxOut (Address (ScriptCredential h) _) _ _ _) <- txInInfos]

-- | Minting policy that ensures the own spending script is invoked in the transaction
{-# INLINEABLE ownForwardingMintingPurpose #-}
ownForwardingMintingPurpose :: (mtx -> [TxInInfo]) -> MintingPurposeType CurrencySymbol red mtx
ownForwardingMintingPurpose toTxInInfos cs = forwardingMintingPurpose cs toTxInInfos cs

-- | Spending script that ensures a given minting script is invoked in the transaction
{-# INLINEABLE forwardingSpendingPurpose #-}
forwardingSpendingPurpose :: (ToScriptHash script) => script -> (txInfo -> MintValue) -> SpendingPurposeType TxOutRef dat red txInfo
forwardingSpendingPurpose (toScriptHash -> ScriptHash hash) toMintValue _ _ _ (mintValueToMap . toMintValue -> mintValue) =
  CurrencySymbol hash `Map.member` mintValue

-- | Spending purpose that ensures the own minting purpose is invoked in the transaction
{-# INLINEABLE ownForwardingSpendingPurpose #-}
ownForwardingSpendingPurpose :: (txInfo -> MintValue) -> (txInfo -> [TxInInfo]) -> SpendingPurposeType TxOutRef dat red txInfo
ownForwardingSpendingPurpose toMintValue toTxInInfos oRef dat red txInfo =
  case [hash | TxInInfo ref (TxOut (Address (ScriptCredential hash) _) _ _ _) <- toTxInInfos txInfo, ref == oRef] of
    [hash] -> forwardingSpendingPurpose hash toMintValue oRef dat red txInfo
    _ -> False

-- * Building multi purpose scripts

{-- Note on building multi-purpose scripts. They are usually built from the
  default-false template and configured purpose by purpose, rather than
  instantiated directly. Most scripts use only a few of the six purposes. For
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
falseTypedMultiPurposeScript =
  TypedMultiPurposeScript
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
