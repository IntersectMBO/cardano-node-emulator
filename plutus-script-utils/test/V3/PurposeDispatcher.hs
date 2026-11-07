{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module V3.PurposeDispatcher (tests) where

import Data.ByteString qualified as BS
import Plutus.Script.Utils.Scripts (ScriptHash (ScriptHash))
import Plutus.Script.Utils.V3.Generators
  ( falseTypedMultiPurposeScript,
    forwardingMintingPurpose,
    forwardingSpendingPurpose,
    ownForwardingMintingPurpose,
    ownForwardingSpendingPurpose,
  )
import Plutus.Script.Utils.V3.Typed
  ( MintingPurposeType,
    SpendingPurposeType,
    TypedMultiPurposeScript
      ( certifyingPurpose,
        mintingPurpose,
        proposingPurpose,
        rewardingPurpose,
        spendingPurpose,
        votingPurpose
      ),
    mkMultiPurposeScript,
  )
import PlutusCore.Evaluation.Machine.ExBudgetingDefaults (defaultCekParametersForTesting)
import PlutusCore.Version (plcVersion110)
import PlutusLedgerApi.V1.Address (Address (Address))
import PlutusLedgerApi.V1.Value qualified as Value
import PlutusLedgerApi.V3
  ( Credential (ScriptCredential),
    CurrencySymbol (CurrencySymbol),
    Datum (Datum),
    MintValue,
    OutputDatum (NoOutputDatum),
    ScriptInfo (MintingScript, SpendingScript),
    TokenName (TokenName),
    TxId (TxId),
    TxInInfo (TxInInfo),
    TxOut (TxOut),
    TxOutRef (TxOutRef),
  )
import PlutusLedgerApi.V3.MintValue (MintValue (UnsafeMintValue))
import PlutusTx
  ( BuiltinData,
    CompiledCode,
    Data (Constr, I),
    compile,
    dataToBuiltinData,
    getPlcNoAnn,
    liftCode,
    toData,
    unsafeApplyCode,
    unsafeFromBuiltinData,
  )
import PlutusTx.AssocMap qualified as Map
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Prelude qualified as P
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase)
import UntypedPlutusCore (Program (Program))
import UntypedPlutusCore.Evaluation.Machine.Cek qualified as Cek
import Prelude (Bool (False, True), Either (Left, Right), Maybe (Just), not, ($))

tests :: TestTree
tests =
  testGroup
    "Plutus.Script.Utils.V3.Typed"
    [ testGroup
        "purpose dispatch"
        [ testCase "certifying" $ succeeds certifyingCode certifyingContext,
          testCase "minting" $ succeeds mintingCode mintingContext,
          testCase "proposing" $ succeeds proposingCode proposingContext,
          testCase "rewarding" $ succeeds rewardingCode rewardingContext,
          testCase "spending with datum" $ succeeds spendingCode spendingContext,
          testCase "spending without datum" $ succeeds spendingNoDatumCode spendingWithoutDatumContext,
          testCase "voting" $ succeeds votingCode votingContext
        ],
      testGroup
        "real ScriptInfo encoding"
        [ testCase "dispatches a real MintingScript context" $
            succeeds realMintingCode realMintingContext,
          testCase "dispatches a real SpendingScript context carrying a Just datum" $
            succeeds realSpendingCode realSpendingContext
        ],
      testGroup
        "non-trivial purpose generators"
        [ testCase "forwarding minting accepts an input at the target script" $
            succeeds forwardingMintingCode $
              forwardingMintingContext targetInput,
          testCase "forwarding minting rejects when target input is absent" $
            fails forwardingMintingCode $
              forwardingMintingContext otherInput,
          testCase "own forwarding minting uses its currency symbol as script hash" $
            succeeds ownForwardingMintingCode $
              ownForwardingMintingContext targetInput,
          testCase "own forwarding minting rejects when own script input is absent" $
            fails ownForwardingMintingCode $
              ownForwardingMintingContext otherInput,
          testCase "forwarding spending accepts when target currency is minted" $
            succeeds forwardingSpendingCode $
              forwardingSpendingContext targetMintValue,
          testCase "forwarding spending rejects when target currency is not minted" $
            fails forwardingSpendingCode $
              forwardingSpendingContext emptyTestMintValue,
          testCase "own forwarding spending accepts a mint by the own script" $
            succeeds ownForwardingSpendingCode $
              ownForwardingSpendingContext targetMintValue targetInput,
          testCase "own forwarding spending rejects when own script does not mint" $
            fails ownForwardingSpendingCode $
              ownForwardingSpendingContext emptyTestMintValue targetInput,
          testCase "own forwarding spending rejects an unrelated input reference" $
            fails ownForwardingSpendingCode $
              ownForwardingSpendingContext targetMintValue otherInput
        ],
      testGroup
        "malformed and unsupported contexts"
        [ testCase "rejects an unsupported purpose" $
            fails falseCode $
              context 6 [I 0] 0 0,
          testCase "rejects an incorrect purpose argument shape" $
            fails mintingCode $
              context 0 [] 0 0,
          testCase "rejects a malformed script context" $
            fails mintingCode $
              dataToBuiltinData $
                Constr 0 [],
          testCase "rejects an invalid redeemer" $
            fails mintingCode $
              contextWithRedeemer 0 [I 21] (Constr 0 []) (I 0)
        ]
    ]

-- Keep the record update outside the PlutusTx quote so the plugin sees a
-- resolved script value, following the pattern used by the V3 generators.
certifyingCode :: CompiledCode (BuiltinData -> P.BuiltinUnit)
certifyingCode = $$(compile [||script||])
  where
    script = mkMultiPurposeScript $ falseTypedMultiPurposeScript {certifyingPurpose = Just certifyingHandler}

mintingCode :: CompiledCode (BuiltinData -> P.BuiltinUnit)
mintingCode = $$(compile [||script||])
  where
    script = mkMultiPurposeScript $ falseTypedMultiPurposeScript {mintingPurpose = Just mintingHandler}

proposingCode :: CompiledCode (BuiltinData -> P.BuiltinUnit)
proposingCode = $$(compile [||script||])
  where
    script = mkMultiPurposeScript $ falseTypedMultiPurposeScript {proposingPurpose = Just proposingHandler}

rewardingCode :: CompiledCode (BuiltinData -> P.BuiltinUnit)
rewardingCode = $$(compile [||script||])
  where
    script = mkMultiPurposeScript $ falseTypedMultiPurposeScript {rewardingPurpose = Just rewardingHandler}

spendingCode :: CompiledCode (BuiltinData -> P.BuiltinUnit)
spendingCode = $$(compile [||script||])
  where
    script = mkMultiPurposeScript $ falseTypedMultiPurposeScript {spendingPurpose = Just spendingHandler}

spendingNoDatumCode :: CompiledCode (BuiltinData -> P.BuiltinUnit)
spendingNoDatumCode = $$(compile [||script||])
  where
    script = mkMultiPurposeScript $ falseTypedMultiPurposeScript {spendingPurpose = Just spendingNoDatumHandler}

votingCode :: CompiledCode (BuiltinData -> P.BuiltinUnit)
votingCode = $$(compile [||script||])
  where
    script = mkMultiPurposeScript $ falseTypedMultiPurposeScript {votingPurpose = Just votingHandler}

-- These two scripts are exercised against contexts whose script-info field is
-- produced by the genuine V3 'ScriptInfo' 'ToData' instance, so they guard the
-- dispatcher against drift in the real constructor tags and in the single-layer
-- 'Maybe' datum encoding.
realMintingCode :: CompiledCode (BuiltinData -> P.BuiltinUnit)
realMintingCode = $$(compile [||script||])
  where
    script =
      mkMultiPurposeScript $
        falseTypedMultiPurposeScript
          { mintingPurpose =
              Just (realMintingHandler :: MintingPurposeType CurrencySymbol BuiltinData BuiltinData)
          }

realSpendingCode :: CompiledCode (BuiltinData -> P.BuiltinUnit)
realSpendingCode = $$(compile [||script||])
  where
    script =
      mkMultiPurposeScript $
        falseTypedMultiPurposeScript
          { spendingPurpose =
              Just (realSpendingHandler :: SpendingPurposeType BuiltinData P.Integer BuiltinData BuiltinData)
          }

falseCode :: CompiledCode (BuiltinData -> P.BuiltinUnit)
falseCode = $$(compile [||script||])
  where
    script = mkMultiPurposeScript falseTypedMultiPurposeScript

forwardingMintingCode :: CompiledCode (BuiltinData -> P.BuiltinUnit)
forwardingMintingCode = $$(compile [||script||])
  where
    script =
      mkMultiPurposeScript $
        falseTypedMultiPurposeScript
          { mintingPurpose =
              Just
                (forwardingMintingPurpose targetCurrencySymbol txInInfosFromData :: MintingPurposeType CurrencySymbol BuiltinData BuiltinData)
          }

ownForwardingMintingCode :: CompiledCode (BuiltinData -> P.BuiltinUnit)
ownForwardingMintingCode = $$(compile [||script||])
  where
    script =
      mkMultiPurposeScript $
        falseTypedMultiPurposeScript
          { mintingPurpose =
              Just
                (ownForwardingMintingPurpose txInInfosFromData :: MintingPurposeType CurrencySymbol BuiltinData BuiltinData)
          }

forwardingSpendingCode :: CompiledCode (BuiltinData -> P.BuiltinUnit)
forwardingSpendingCode = $$(compile [||script||])
  where
    script =
      mkMultiPurposeScript $
        falseTypedMultiPurposeScript
          { spendingPurpose =
              Just
                (forwardingSpendingPurpose targetCurrencySymbol mintValueFromData :: SpendingPurposeType TxOutRef BuiltinData BuiltinData BuiltinData)
          }

ownForwardingSpendingCode :: CompiledCode (BuiltinData -> P.BuiltinUnit)
ownForwardingSpendingCode = $$(compile [||script||])
  where
    script =
      mkMultiPurposeScript $
        falseTypedMultiPurposeScript
          { spendingPurpose =
              Just
                (ownForwardingSpendingPurpose mintValueFromView inputsFromView :: SpendingPurposeType TxOutRef BuiltinData BuiltinData BuiltinData)
          }

{-# INLINEABLE mintValueFromView #-}
mintValueFromView :: BuiltinData -> MintValue
mintValueFromView dat = P.fst (unsafeFromBuiltinData dat :: (MintValue, [TxInInfo]))

{-# INLINEABLE inputsFromView #-}
inputsFromView :: BuiltinData -> [TxInInfo]
inputsFromView dat = P.snd (unsafeFromBuiltinData dat :: (MintValue, [TxInInfo]))

{-# INLINEABLE txInInfosFromData #-}
txInInfosFromData :: BuiltinData -> [TxInInfo]
txInInfosFromData = unsafeFromBuiltinData

{-# INLINEABLE mintValueFromData #-}
mintValueFromData :: BuiltinData -> MintValue
mintValueFromData = unsafeFromBuiltinData

{-# INLINEABLE certifyingHandler #-}
certifyingHandler :: P.Integer -> P.Integer -> P.Integer -> P.Integer -> P.Bool
certifyingHandler index certificate redeemer txInfo =
  index
    P.== 11
    P.&& certificate
    P.== 12
    P.&& redeemer
    P.== 13
    P.&& txInfo
    P.== 14

{-# INLINEABLE mintingHandler #-}
mintingHandler :: P.Integer -> P.Integer -> P.Integer -> P.Bool
mintingHandler currencySymbol redeemer txInfo =
  currencySymbol
    P.== 21
    P.&& redeemer
    P.== 22
    P.&& txInfo
    P.== 23

{-# INLINEABLE proposingHandler #-}
proposingHandler :: P.Integer -> P.Integer -> P.Integer -> P.Integer -> P.Bool
proposingHandler index proposal redeemer txInfo =
  index
    P.== 31
    P.&& proposal
    P.== 32
    P.&& redeemer
    P.== 33
    P.&& txInfo
    P.== 34

{-# INLINEABLE rewardingHandler #-}
rewardingHandler :: P.Integer -> P.Integer -> P.Integer -> P.Bool
rewardingHandler credential redeemer txInfo =
  credential
    P.== 41
    P.&& redeemer
    P.== 42
    P.&& txInfo
    P.== 43

{-# INLINEABLE spendingHandler #-}
spendingHandler :: P.Integer -> P.Maybe P.Integer -> P.Integer -> P.Integer -> P.Bool
spendingHandler outRef datum redeemer txInfo =
  outRef
    P.== 51
    P.&& datum
    P.== P.Just 52
    P.&& redeemer
    P.== 53
    P.&& txInfo
    P.== 54

{-# INLINEABLE spendingNoDatumHandler #-}
spendingNoDatumHandler :: P.Integer -> P.Maybe P.Integer -> P.Integer -> P.Integer -> P.Bool
spendingNoDatumHandler outRef datum redeemer txInfo =
  outRef
    P.== 51
    P.&& datum
    P.== P.Nothing
    P.&& redeemer
    P.== 53
    P.&& txInfo
    P.== 54

{-# INLINEABLE votingHandler #-}
votingHandler :: P.Integer -> P.Integer -> P.Integer -> P.Bool
votingHandler voter redeemer txInfo =
  voter
    P.== 61
    P.&& redeemer
    P.== 62
    P.&& txInfo
    P.== 63

{-# INLINEABLE realMintingHandler #-}
realMintingHandler :: CurrencySymbol -> BuiltinData -> BuiltinData -> P.Bool
realMintingHandler currencySymbol _ _ = currencySymbol P.== targetCurrencySymbol

{-# INLINEABLE realSpendingHandler #-}
realSpendingHandler :: BuiltinData -> P.Maybe P.Integer -> BuiltinData -> BuiltinData -> P.Bool
realSpendingHandler _ datum _ _ = datum P.== P.Just 52

-- These small contexts use the real V3 'ScriptInfo' constructor tags
-- (Minting=0, Spending=1, Rewarding=2, Certifying=3, Voting=4, Proposing=5)
-- and field order, while keeping the test-specific TxInfo and redeemer
-- payloads easy to distinguish. The spending datum is a 'Maybe' wrapped in a
-- single 'Constr' layer: @Just d@ is @Constr 0 [d]@ and @Nothing@ is
-- @Constr 1 []@.
certifyingContext, mintingContext, proposingContext, rewardingContext, spendingContext, spendingWithoutDatumContext, votingContext :: BuiltinData
mintingContext = context 0 [I 21] 22 23
spendingContext = context 1 [I 51, Constr 0 [I 52]] 53 54
spendingWithoutDatumContext = context 1 [I 51, Constr 1 []] 53 54
rewardingContext = context 2 [I 41] 42 43
certifyingContext = context 3 [I 11, I 12] 13 14
votingContext = context 4 [I 61] 62 63
proposingContext = context 5 [I 31, I 32] 33 34

-- The script-info field here is the real V3 'ScriptInfo' 'ToData' output, so a
-- change to the library tag order or to the datum encoding would break these.
realMintingContext :: BuiltinData
realMintingContext =
  dataToBuiltinData $ Constr 0 [I 0, I 0, toData (MintingScript targetCurrencySymbol)]

realSpendingContext :: BuiltinData
realSpendingContext =
  dataToBuiltinData $
    Constr 0 [I 0, I 0, toData (SpendingScript targetRef (Just (Datum (dataToBuiltinData (I 52)))))]

targetScriptHash :: ScriptHash
targetScriptHash = ScriptHash $ Builtins.toBuiltin $ BS.pack [1 .. 28]

otherScriptHash :: ScriptHash
otherScriptHash = ScriptHash $ Builtins.toBuiltin $ BS.pack [29 .. 56]

-- Built as a 'BuiltinByteString' literal (bytes 1..28) rather than via
-- 'BS.pack', so the plugin can compile it when it is baked into the forwarding
-- scripts as their forwarding target.
targetCurrencySymbol :: CurrencySymbol
targetCurrencySymbol = CurrencySymbol "\1\2\3\4\5\6\7\8\9\10\11\12\13\14\15\16\17\18\19\20\21\22\23\24\25\26\27\28"

targetTokenName :: TokenName
targetTokenName = TokenName $ Builtins.toBuiltin $ BS.pack [116, 101, 115, 116, 45, 116, 111, 107, 101, 110]

targetMintValue :: MintValue
targetMintValue =
  UnsafeMintValue $
    Map.singleton targetCurrencySymbol $
      Map.singleton targetTokenName 1

emptyTestMintValue :: MintValue
emptyTestMintValue = UnsafeMintValue Map.empty

targetRef, otherRef :: TxOutRef
targetRef = TxOutRef (TxId $ Builtins.toBuiltin $ BS.pack [57 .. 88]) 0
otherRef = TxOutRef (TxId $ Builtins.toBuiltin $ BS.pack [89 .. 120]) 1

targetInput, otherInput :: TxInInfo
targetInput = inputAt targetRef targetScriptHash
otherInput = inputAt otherRef otherScriptHash

inputAt :: TxOutRef -> ScriptHash -> TxInInfo
inputAt ref (ScriptHash hash) =
  TxInInfo
    ref
    ( TxOut
        (Address (ScriptCredential (ScriptHash hash)) P.Nothing)
        (Value.singleton Value.adaSymbol Value.adaToken 0)
        NoOutputDatum
        P.Nothing
    )

forwardingMintingContext :: TxInInfo -> BuiltinData
forwardingMintingContext input =
  contextWithRedeemer 0 [toData targetCurrencySymbol] (I 0) (toData [input])

ownForwardingMintingContext :: TxInInfo -> BuiltinData
ownForwardingMintingContext input =
  contextWithRedeemer 0 [toData targetCurrencySymbol] (I 0) (toData [input])

forwardingSpendingContext :: MintValue -> BuiltinData
forwardingSpendingContext mintValue =
  contextWithRedeemer 1 [toData targetRef, Constr 1 []] (I 0) (toData mintValue)

ownForwardingSpendingContext :: MintValue -> TxInInfo -> BuiltinData
ownForwardingSpendingContext mintValue input =
  contextWithRedeemer
    1
    [toData targetRef, Constr 1 []]
    (I 0)
    (toData (mintValue, [input]))

context :: P.Integer -> [Data] -> P.Integer -> P.Integer -> BuiltinData
context purpose arguments redeemer txInfo =
  contextWithRedeemer purpose arguments (I redeemer) (I txInfo)

contextWithRedeemer :: P.Integer -> [Data] -> Data -> Data -> BuiltinData
contextWithRedeemer purpose arguments redeemer txInfo =
  dataToBuiltinData $ Constr 0 [txInfo, redeemer, Constr purpose arguments]

-- Evaluate the generated UPLC with PlutusTx's CEK test parameters; this
-- exercises the dispatcher without constructing a ledger cost model.
succeeds :: CompiledCode (BuiltinData -> P.BuiltinUnit) -> BuiltinData -> Assertion
succeeds code input =
  assertBool "expected UPLC evaluation to succeed" $ evaluateCode code input

fails :: CompiledCode (BuiltinData -> P.BuiltinUnit) -> BuiltinData -> Assertion
fails code input =
  assertBool "expected UPLC evaluation to fail" $ not $ evaluateCode code input

-- Apply the context as a compiled UPLC constant and run the resulting term
-- directly through CEK, avoiding ledger-level evaluation and cost models.
evaluateCode :: CompiledCode (BuiltinData -> P.BuiltinUnit) -> BuiltinData -> Bool
evaluateCode code input =
  let Program _ _ term =
        getPlcNoAnn $
          unsafeApplyCode code $
            liftCode plcVersion110 input
      (result, _, _) =
        Cek.runCekDeBruijn
          defaultCekParametersForTesting
          Cek.counting
          Cek.noEmitter
          term
   in case result of
        Left _ -> False
        Right _ -> True
