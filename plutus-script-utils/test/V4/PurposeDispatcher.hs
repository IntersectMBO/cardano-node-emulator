{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module V4.PurposeDispatcher (tests) where

import Data.ByteString qualified as BS
import Plutus.Script.Utils.V4.Generators
  ( falseTypedMultiPurposeScript,
    forwardingMintingPurpose,
    forwardingSpendingPurpose,
    ownForwardingMintingPurpose,
    ownForwardingSpendingPurpose,
  )
import Plutus.Script.Utils.V4.Typed
  ( CertifyingPurposeType,
    GuardingPurposeType,
    MintingPurposeType,
    ProposingPurposeType,
    SpendingPurposeType,
    TypedMultiPurposeScript
      ( certifyingPurpose,
        guardingPurpose,
        mintingPurpose,
        proposingPurpose,
        spendingPurpose,
        votingPurpose,
        withdrawingPurpose
      ),
    VotingPurposeType,
    WithdrawingPurposeType,
    mkMultiPurposeScript,
  )
import PlutusCore.Evaluation.Machine.ExBudgetingDefaults (defaultCekParametersForTesting)
import PlutusCore.Version (plcVersion110)
import PlutusLedgerApi.V3.MintValue (MintValue (UnsafeMintValue))
import PlutusLedgerApi.V4
  ( Address (Address),
    Credential (ScriptCredential),
    CurrencySymbol (CurrencySymbol),
    Datum (Datum),
    OutputDatum (NoOutputDatum),
    ScriptHash (ScriptHash),
    ScriptInfo (MintingScript, SpendingScript),
    TokenName (TokenName),
    TxId (TxId),
    TxInInfo (TxInInfo),
    TxOut (TxOut),
    TxOutRef (TxOutRef),
  )
import PlutusLedgerApi.V4.Value qualified as Value
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
import UntypedPlutusCore.Core.Type qualified as Cek
import UntypedPlutusCore.Evaluation.Machine.Cek qualified as Cek
import Prelude (Bool (False, True), Maybe (Just, Nothing), not, ($), (.))

tests :: TestTree
tests =
  testGroup
    "Plutus.Script.Utils.V4.Typed"
    [ testGroup
        "purpose dispatch"
        [ testCase "certifying" $ succeeds certifyingCode certifyingContext,
          testCase "minting" $ succeeds mintingCode mintingContext,
          testCase "proposing" $ succeeds proposingCode proposingContext,
          testCase "withdrawing" $ succeeds withdrawingCode withdrawingContext,
          testCase "spending with datum" $ succeeds spendingCode spendingContext,
          testCase "spending without datum" $ succeeds spendingNoDatumCode spendingWithoutDatumContext,
          testCase "voting" $ succeeds votingCode votingContext,
          testCase "guarding with top-level tx info" $ succeeds guardingCode guardingContext
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
              context 7 [] 0 0,
          testCase "rejects an incorrect purpose argument shape" $
            fails mintingCode $
              context 0 [] 0 0,
          testCase "rejects a malformed script context" $
            fails mintingCode $
              dataToBuiltinData $
                Constr 0 [],
          testCase "rejects an invalid redeemer" $
            fails mintingCode $
              contextWithRedeemer 0 [I 21] (Constr 0 []) (I 0) testScriptHash
        ]
    ]

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

withdrawingCode :: CompiledCode (BuiltinData -> P.BuiltinUnit)
withdrawingCode = $$(compile [||script||])
  where
    script =
      mkMultiPurposeScript $ falseTypedMultiPurposeScript {withdrawingPurpose = Just withdrawingHandler}

spendingCode :: CompiledCode (BuiltinData -> P.BuiltinUnit)
spendingCode = $$(compile [||script||])
  where
    script = mkMultiPurposeScript $ falseTypedMultiPurposeScript {spendingPurpose = Just spendingHandler}

spendingNoDatumCode :: CompiledCode (BuiltinData -> P.BuiltinUnit)
spendingNoDatumCode = $$(compile [||script||])
  where
    script =
      mkMultiPurposeScript $ falseTypedMultiPurposeScript {spendingPurpose = Just spendingNoDatumHandler}

votingCode :: CompiledCode (BuiltinData -> P.BuiltinUnit)
votingCode = $$(compile [||script||])
  where
    script = mkMultiPurposeScript $ falseTypedMultiPurposeScript {votingPurpose = Just votingHandler}

guardingCode :: CompiledCode (BuiltinData -> P.BuiltinUnit)
guardingCode = $$(compile [||script||])
  where
    script = mkMultiPurposeScript $ falseTypedMultiPurposeScript {guardingPurpose = Just guardingHandler}

realMintingCode :: CompiledCode (BuiltinData -> P.BuiltinUnit)
realMintingCode = $$(compile [||script||])
  where
    script =
      mkMultiPurposeScript $
        falseTypedMultiPurposeScript
          { mintingPurpose =
              Just
                ( realMintingHandler ::
                    MintingPurposeType ScriptHash CurrencySymbol BuiltinData BuiltinData
                )
          }

realSpendingCode :: CompiledCode (BuiltinData -> P.BuiltinUnit)
realSpendingCode = $$(compile [||script||])
  where
    script =
      mkMultiPurposeScript $
        falseTypedMultiPurposeScript
          { spendingPurpose =
              Just
                ( realSpendingHandler ::
                    SpendingPurposeType ScriptHash TxOutRef P.Integer BuiltinData BuiltinData
                )
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
                ( forwardingMintingPurpose targetScriptHash txInInfosFromData ::
                    MintingPurposeType ScriptHash CurrencySymbol BuiltinData BuiltinData
                )
          }

ownForwardingMintingCode :: CompiledCode (BuiltinData -> P.BuiltinUnit)
ownForwardingMintingCode = $$(compile [||script||])
  where
    script =
      mkMultiPurposeScript $
        falseTypedMultiPurposeScript
          { mintingPurpose =
              Just
                ( ownForwardingMintingPurpose txInInfosFromData ::
                    MintingPurposeType ScriptHash CurrencySymbol BuiltinData BuiltinData
                )
          }

forwardingSpendingCode :: CompiledCode (BuiltinData -> P.BuiltinUnit)
forwardingSpendingCode = $$(compile [||script||])
  where
    script =
      mkMultiPurposeScript $
        falseTypedMultiPurposeScript
          { spendingPurpose =
              Just
                ( forwardingSpendingPurpose targetCurrencySymbol mintValueFromData ::
                    SpendingPurposeType ScriptHash TxOutRef BuiltinData BuiltinData BuiltinData
                )
          }

ownForwardingSpendingCode :: CompiledCode (BuiltinData -> P.BuiltinUnit)
ownForwardingSpendingCode = $$(compile [||script||])
  where
    script =
      mkMultiPurposeScript $
        falseTypedMultiPurposeScript
          { spendingPurpose =
              Just
                ( ownForwardingSpendingPurpose mintValueFromView inputsFromView ::
                    SpendingPurposeType ScriptHash TxOutRef BuiltinData BuiltinData BuiltinData
                )
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
certifyingHandler :: CertifyingPurposeType ScriptHash P.Integer P.Integer P.Integer P.Integer
certifyingHandler scriptHash index certificate redeemer txInfo =
  scriptHash
    P.== testScriptHash
    P.&& index
    P.== 5
    P.&& certificate
    P.== 6
    P.&& redeemer
    P.== 2
    P.&& txInfo
    P.== 3

{-# INLINEABLE mintingHandler #-}
mintingHandler :: MintingPurposeType ScriptHash P.Integer P.Integer P.Integer
mintingHandler scriptHash currencySymbol redeemer txInfo =
  scriptHash
    P.== testScriptHash
    P.&& currencySymbol
    P.== 1
    P.&& redeemer
    P.== 2
    P.&& txInfo
    P.== 3

{-# INLINEABLE proposingHandler #-}
proposingHandler :: ProposingPurposeType ScriptHash P.Integer P.Integer P.Integer P.Integer
proposingHandler scriptHash index proposal redeemer txInfo =
  scriptHash
    P.== testScriptHash
    P.&& index
    P.== 8
    P.&& proposal
    P.== 9
    P.&& redeemer
    P.== 2
    P.&& txInfo
    P.== 3

{-# INLINEABLE withdrawingHandler #-}
withdrawingHandler :: WithdrawingPurposeType ScriptHash P.Integer P.Integer P.Integer
withdrawingHandler scriptHash accountId redeemer txInfo =
  scriptHash
    P.== testScriptHash
    P.&& accountId
    P.== 4
    P.&& redeemer
    P.== 2
    P.&& txInfo
    P.== 3

{-# INLINEABLE spendingHandler #-}
spendingHandler :: SpendingPurposeType ScriptHash P.Integer P.Integer P.Integer P.Integer
spendingHandler scriptHash outRef datum redeemer txInfo =
  scriptHash
    P.== testScriptHash
    P.&& outRef
    P.== 2
    P.&& (datum P.== Nothing P.|| datum P.== Just 12)
    P.&& redeemer
    P.== 2
    P.&& txInfo
    P.== 3

{-# INLINEABLE spendingNoDatumHandler #-}
spendingNoDatumHandler :: SpendingPurposeType ScriptHash P.Integer P.Integer P.Integer P.Integer
spendingNoDatumHandler scriptHash outRef datum redeemer txInfo =
  scriptHash
    P.== testScriptHash
    P.&& outRef
    P.== 2
    P.&& datum
    P.== Nothing
    P.&& redeemer
    P.== 2
    P.&& txInfo
    P.== 3

{-# INLINEABLE votingHandler #-}
votingHandler :: VotingPurposeType ScriptHash P.Integer P.Integer P.Integer
votingHandler scriptHash voter redeemer txInfo =
  scriptHash
    P.== testScriptHash
    P.&& voter
    P.== 7
    P.&& redeemer
    P.== 2
    P.&& txInfo
    P.== 3

{-# INLINEABLE guardingHandler #-}
guardingHandler :: GuardingPurposeType ScriptHash P.Integer P.Integer P.Integer P.Integer
guardingHandler scriptHash index topTxInfo redeemer txInfo =
  scriptHash
    P.== testScriptHash
    P.&& index
    P.== 10
    P.&& topTxInfo
    P.== Just 11
    P.&& redeemer
    P.== 2
    P.&& txInfo
    P.== 3

{-# INLINEABLE realMintingHandler #-}
realMintingHandler :: ScriptHash -> CurrencySymbol -> BuiltinData -> BuiltinData -> P.Bool
realMintingHandler scriptHash currencySymbol _ _ =
  scriptHash
    P.== targetScriptHash
    P.&& currencySymbol
    P.== targetCurrencySymbol

{-# INLINEABLE realSpendingHandler #-}
realSpendingHandler ::
  ScriptHash ->
  TxOutRef ->
  P.Maybe P.Integer ->
  BuiltinData ->
  BuiltinData ->
  P.Bool
realSpendingHandler _ _ datum _ _ = datum P.== P.Just 52

-- These contexts use real V4 'ScriptInfo' encodings while keeping the
-- transaction-info and redeemer payloads small and distinguishable.
certifyingContext,
  mintingContext,
  proposingContext,
  withdrawingContext,
  spendingContext,
  spendingWithoutDatumContext,
  votingContext,
  guardingContext ::
    BuiltinData
mintingContext = context 0 [I 1] 2 3
spendingContext = context 1 [I 2, Constr 0 [I 12]] 2 3
spendingWithoutDatumContext = context 1 [I 2, Constr 1 []] 2 3
withdrawingContext = context 2 [I 4] 2 3
certifyingContext = context 3 [I 5, I 6] 2 3
votingContext = context 4 [I 7] 2 3
proposingContext = context 5 [I 8, I 9] 2 3
guardingContext = context 6 [I 10, Constr 0 [I 11]] 2 3

realMintingContext :: BuiltinData
realMintingContext =
  dataToBuiltinData $
    Constr
      0
      [ I 0,
        I 0,
        toData (MintingScript targetCurrencySymbol),
        toData targetScriptHash
      ]

realSpendingContext :: BuiltinData
realSpendingContext =
  dataToBuiltinData $
    Constr
      0
      [ I 0,
        I 0,
        toData (SpendingScript targetRef (Just (Datum (dataToBuiltinData (I 52))))),
        toData targetScriptHash
      ]

targetScriptHash, otherScriptHash :: ScriptHash
targetScriptHash = ScriptHash "\1\2\3\4\5\6\7\8\9\10\11\12\13\14\15\16\17\18\19\20\21\22\23\24\25\26\27\28"
otherScriptHash = ScriptHash "\29\30\31\32\33\34\35\36\37\38\39\40\41\42\43\44\45\46\47\48\49\50\51\52\53\54\55\56"

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
inputAt ref scriptHash =
  TxInInfo
    ref
    ( TxOut
        (Address (ScriptCredential scriptHash) P.Nothing)
        (Value.singleton Value.adaSymbol Value.adaToken 0)
        NoOutputDatum
        P.Nothing
    )

forwardingMintingContext :: TxInInfo -> BuiltinData
forwardingMintingContext input =
  contextWithRedeemer 0 [toData targetCurrencySymbol] (I 0) (toData [input]) testScriptHash

ownForwardingMintingContext :: TxInInfo -> BuiltinData
ownForwardingMintingContext input =
  contextWithRedeemer 0 [toData targetCurrencySymbol] (I 0) (toData [input]) testScriptHash

forwardingSpendingContext :: MintValue -> BuiltinData
forwardingSpendingContext mintValue =
  contextWithRedeemer 1 [toData targetRef, Constr 1 []] (I 0) (toData mintValue) testScriptHash

ownForwardingSpendingContext :: MintValue -> TxInInfo -> BuiltinData
ownForwardingSpendingContext mintValue input =
  contextWithRedeemer
    1
    [toData targetRef, Constr 1 []]
    (I 0)
    (toData (mintValue, [input]))
    testScriptHash

context :: P.Integer -> [Data] -> P.Integer -> P.Integer -> BuiltinData
context purpose arguments redeemer txInfo =
  contextWithRedeemer purpose arguments (I redeemer) (I txInfo) testScriptHash

contextWithRedeemer :: P.Integer -> [Data] -> Data -> Data -> ScriptHash -> BuiltinData
contextWithRedeemer purpose arguments redeemer txInfo scriptHash =
  dataToBuiltinData $
    Constr
      0
      [ txInfo,
        redeemer,
        Constr purpose arguments,
        toData scriptHash
      ]

testScriptHash :: ScriptHash
testScriptHash = ScriptHash "test-script-hash"

succeeds :: CompiledCode (BuiltinData -> P.BuiltinUnit) -> BuiltinData -> Assertion
succeeds code input =
  assertBool "expected UPLC evaluation to succeed" $ evaluateCode code input

fails :: CompiledCode (BuiltinData -> P.BuiltinUnit) -> BuiltinData -> Assertion
fails code input =
  assertBool "expected UPLC evaluation to fail" $ not $ evaluateCode code input

evaluateCode :: CompiledCode (BuiltinData -> P.BuiltinUnit) -> BuiltinData -> Bool
evaluateCode code =
  \case Cek.CekFailure _ -> False; _ -> True
    . Cek._cekReportResult
    . Cek.runCekDeBruijn defaultCekParametersForTesting Cek.counting Cek.noEmitter
    . Cek._progTerm
    . getPlcNoAnn
    . unsafeApplyCode code
    . liftCode plcVersion110
