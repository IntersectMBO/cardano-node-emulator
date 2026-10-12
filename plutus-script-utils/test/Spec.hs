{-# LANGUAGE TemplateHaskell #-}

-- | Tests for 'Plutus.Script.Utils.V3.TypedTH.mkMultiPurposeScriptFor'.
--
-- The TH splice produces an ordinary Haskell function before it ever
-- reaches 'PlutusTx.compile', so it's exercised directly here, with
-- ordinary Haskell exceptions standing in for on-chain validation failure
-- ('PlutusTx.Prelude.traceError'/'PlutusTx.Prelude.check' both reduce to
-- plain 'error' outside the plugin).
module Main (main) where

import Control.Exception (SomeException, evaluate, try)
import Plutus.Script.Utils.V3.Typed
  ( ScriptContextResolvedScriptInfo (ScriptContextResolvedScriptInfo),
    TypedMultiPurposeScript (TypedMultiPurposeScript),
  )
import Plutus.Script.Utils.V3.TypedTH (mkMultiPurposeScriptFor)
import PlutusLedgerApi.V3
  ( BuiltinData,
    CurrencySymbol (CurrencySymbol),
    Datum (Datum),
    ScriptInfo (MintingScript, SpendingScript),
    TxId (TxId),
    TxOutRef (TxOutRef),
  )
import PlutusTx (ToData (toBuiltinData))
import PlutusTx.Prelude (BuiltinUnit)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase)

main :: IO ()
main = defaultMain tests

mintHandlerTrue, mintHandlerFalse :: CurrencySymbol -> () -> () -> Bool
mintHandlerTrue _ _ _ = True
mintHandlerFalse _ _ _ = False

spendHandlerTrue :: TxOutRef -> Maybe () -> () -> () -> Bool
spendHandlerTrue _ _ _ _ = True

-- Purposes are read off the literal @TypedMultiPurposeScript ..@ written
-- directly at each splice site, so every configuration here needs its own
-- generated function — there's no longer a separate list to vary at runtime.
-- Every handler here is already concrete (not type-applied), so the only
-- ambiguity is in the always-@Nothing@ slots; annotating the whole thing
-- as @()@ throughout resolves them without affecting the active purpose.
type AllUnit = TypedMultiPurposeScript () () () () () () () () () () () () ()

mintTrue, mintFalse :: BuiltinData -> BuiltinUnit
mintTrue = $(mkMultiPurposeScriptFor [|TypedMultiPurposeScript Nothing (Just mintHandlerTrue) Nothing Nothing Nothing Nothing :: AllUnit|])
mintFalse = $(mkMultiPurposeScriptFor [|TypedMultiPurposeScript Nothing (Just mintHandlerFalse) Nothing Nothing Nothing Nothing :: AllUnit|])

spendTrue :: BuiltinData -> BuiltinUnit
spendTrue = $(mkMultiPurposeScriptFor [|TypedMultiPurposeScript Nothing Nothing Nothing Nothing (Just spendHandlerTrue) Nothing :: AllUnit|])

noPurposesGen :: BuiltinData -> BuiltinUnit
noPurposesGen = $(mkMultiPurposeScriptFor [|TypedMultiPurposeScript Nothing Nothing Nothing Nothing Nothing Nothing :: AllUnit|])

encodeInput :: ScriptInfo -> BuiltinData
encodeInput info = toBuiltinData (ScriptContextResolvedScriptInfo (toBuiltinData ()) (toBuiltinData ()) info)

mintingInput :: BuiltinData
mintingInput = encodeInput (MintingScript (CurrencySymbol ""))

sampleTxOutRef :: TxOutRef
sampleTxOutRef = TxOutRef (TxId "") 0

spendingInputNoDatum, spendingInputWithDatum :: BuiltinData
spendingInputNoDatum = encodeInput (SpendingScript sampleTxOutRef Nothing)
spendingInputWithDatum = encodeInput (SpendingScript sampleTxOutRef (Just (Datum (toBuiltinData ()))))

assertSucceeds, assertFails :: BuiltinUnit -> Assertion
assertSucceeds x = do
  result <- try (evaluate (x `seq` ())) :: IO (Either SomeException ())
  either (assertFailure . ("expected success, got: " <>) . show) pure result
assertFails x = do
  result <- try (evaluate (x `seq` ())) :: IO (Either SomeException ())
  either (const (pure ())) (const (assertFailure "expected failure, got success")) result

tests :: TestTree
tests =
  testGroup
    "Plutus.Script.Utils.V3.TypedTH"
    [ testCase "minting handler returns True" $
        assertSucceeds (mintTrue mintingInput),
      testCase "minting handler returns False" $
        assertFails (mintFalse mintingInput),
      testCase "purpose not implemented by the script: no branch exists" $
        assertFails (mintTrue spendingInputNoDatum),
      testCase "spending with a present datum" $
        assertSucceeds (spendTrue spendingInputWithDatum),
      testCase "spending with no datum" $
        assertSucceeds (spendTrue spendingInputNoDatum),
      testCase "no purposes implemented: always fails" $
        assertFails (noPurposesGen mintingInput)
    ]
