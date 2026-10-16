{-# LANGUAGE NoImplicitPrelude #-}

-- | Example\/test purpose handlers and sample scripts. The general-purpose
-- script-building combinators ('falseTypedMultiPurposeScript',
-- 'withMintingPurpose', etc.) have moved to
-- "Plutus.Script.Utils.V3.Typed" — re-exported here so existing imports
-- from this module keep working.
module Plutus.Script.Utils.V3.Generators
  ( trueCertifyingPurpose,
    trueMintingPurpose,
    forwardingMintingPurpose,
    ownForwardingMintingPurpose,
    trueProposingPurpose,
    trueRewardingPurpose,
    trueSpendingPurpose,
    forwardingSpendingPurpose,
    ownForwardingSpendingPurpose,
    trueVotingPurpose,
    falseTypedMultiPurposeScript,
    trueTypedMultiPurposeScript,
    withCertifyingPurpose,
    addCertifyingPurpose,
    withMintingPurpose,
    addMintingPurpose,
    withProposingPurpose,
    addProposingPurpose,
    withRewardingPurpose,
    addRewardingPurpose,
    withSpendingPurpose,
    addSpendingPurpose,
    withVotingPurpose,
    addVotingPurpose,
    trueMintingMPScript,
    trueSpendingMPScript,
    falseMPScript,
    trueMPScript,
    multiPurposeScriptValue,
  )
where

import Data.Maybe (Maybe (Just, Nothing))
import Plutus.Script.Utils.Scripts
  ( ScriptHash (ScriptHash),
    ToScript (toScript),
    ToScriptHash (toScriptHash),
    toCurrencySymbol,
  )
import Plutus.Script.Utils.V3.Typed
  ( MintingPurposeType',
    MultiPurposeScript (MultiPurposeScript),
    SpendingPurposeType',
    addCertifyingPurpose,
    addMintingPurpose,
    addProposingPurpose,
    addRewardingPurpose,
    addSpendingPurpose,
    addVotingPurpose,
    falseTypedMultiPurposeScript,
    trueCertifyingPurpose,
    trueMintingPurpose,
    trueProposingPurpose,
    trueRewardingPurpose,
    trueSpendingPurpose,
    trueTypedMultiPurposeScript,
    trueVotingPurpose,
    withCertifyingPurpose,
    withMintingPurpose,
    withProposingPurpose,
    withRewardingPurpose,
    withSpendingPurpose,
    withVotingPurpose,
  )
import Plutus.Script.Utils.V3.TypedTH (mkMultiPurposeScriptFor)
import PlutusLedgerApi.V1.Address (Address (Address))
import PlutusLedgerApi.V3
  ( BuiltinData,
    Credential (ScriptCredential),
    CurrencySymbol (CurrencySymbol),
    MintValue,
    TokenName,
    TxInInfo (TxInInfo),
    TxOut (TxOut),
    Value,
    mintValueToMap,
    singleton,
  )
import PlutusTx.AssocMap qualified as Map
import PlutusTx.List (elem)
import PlutusTx.Prelude
  ( Bool (False, True),
    BuiltinUnit,
    Eq ((==)),
    Integer,
    ($),
    (.),
  )
import PlutusTx.TH (compile)

-- | Minting script that ensures a given spending script is invoked in the transaction
{-# INLINEABLE forwardingMintingPurpose #-}
forwardingMintingPurpose :: (ToScriptHash script) => script -> (mtx -> [TxInInfo]) -> MintingPurposeType' red mtx
forwardingMintingPurpose (toScriptHash -> sHash) toTxInInfos _ _ (toTxInInfos -> txInInfos) =
  sHash `elem` [h | TxInInfo _ (TxOut (Address (ScriptCredential h) _) _ _ _) <- txInInfos]

-- | Minting policy that ensures the own spending script is invoked in the transaction
{-# INLINEABLE ownForwardingMintingPurpose #-}
ownForwardingMintingPurpose :: (mtx -> [TxInInfo]) -> MintingPurposeType' red mtx
ownForwardingMintingPurpose toTxInInfos cs = forwardingMintingPurpose cs toTxInInfos cs

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
trueMintingMPScript = MultiPurposeScript $ toScript $$(compile [||validate||])
  where
    validate =
      $( mkMultiPurposeScriptFor
           [|falseTypedMultiPurposeScript `withMintingPurpose` (trueMintingPurpose @() @())|]
       )

-- | The multi-purpose script that returns @True@ in spending purpose and @False@
-- otherwise
trueSpendingMPScript :: MultiPurposeScript a
trueSpendingMPScript = MultiPurposeScript $ toScript $$(compile [||validate||])
  where
    validate =
      $( mkMultiPurposeScriptFor
           [|falseTypedMultiPurposeScript `withSpendingPurpose` trueSpendingPurpose @() @() @()|]
       )

-- | The multi-purpose script that returns @True@ in all purposes
trueMPScript :: MultiPurposeScript a
trueMPScript = MultiPurposeScript $ toScript $$(compile [||validate||])
  where
    validate = $(mkMultiPurposeScriptFor [|trueTypedMultiPurposeScript|])

falseMPScript :: MultiPurposeScript a
falseMPScript = MultiPurposeScript $ toScript $$(compile [||validate||])
  where
    -- No active purposes generates a fully polymorphic `\_ -> check False`;
    -- nothing else here pins its argument type, so it needs an explicit one.
    validate :: BuiltinData -> BuiltinUnit
    validate = $(mkMultiPurposeScriptFor [|falseTypedMultiPurposeScript|])

multiPurposeScriptValue :: MultiPurposeScript a -> TokenName -> Integer -> Value
multiPurposeScriptValue mpScript = singleton (toCurrencySymbol mpScript)
