{-# LANGUAGE NoImplicitPrelude #-}

module ShowBSSpec (tests) where

import Plutus.Script.Utils.ShowBS (ShowBS (showBS))
import Plutus.Script.Utils.V1.ShowBS ()
import Plutus.Script.Utils.V2.ShowBS ()
import Plutus.Script.Utils.V3.ShowBS ()
import PlutusCore.Evaluation.Machine.ExBudgetingDefaults (defaultCekParametersForTesting)
import PlutusCore.Version (plcVersion110)
import PlutusLedgerApi.V1 qualified as V1
import PlutusLedgerApi.V2 qualified as V2
import PlutusLedgerApi.V3 qualified as V3
import PlutusTx
  ( BuiltinData,
    CompiledCode,
    Data (B, Constr, I, List, Map),
    compile,
    dataToBuiltinData,
    getPlcNoAnn,
    liftCode,
    unsafeApplyCode,
  )
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Builtins (BuiltinByteString)
import PlutusTx.Prelude qualified as P
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)
import UntypedPlutusCore (Program (Program))
import UntypedPlutusCore.Evaluation.Machine.Cek qualified as Cek
import Prelude (Bool (False, True), Either (Left, Right), IO, Integer, Maybe, ($))

tests :: TestTree
tests =
  testGroup
    "Plutus.Script.Utils.ShowBS"
    [ testGroup
        "common instances"
        [ testCase "integers" $ succeeds integerCode unitData,
          testCase "byte strings use lower-case base16" $ succeeds byteStringCode unitData,
          testCase "containers" $ succeeds containerCode unitData
        ],
      testGroup
        "BuiltinData"
        [ testCase "Constr" $ succeeds constrDataCode (dataToBuiltinData (Constr 0 [I 1])),
          testCase "Map" $ succeeds mapDataCode (dataToBuiltinData (Map [(I 1, B "a")])),
          testCase "List" $ succeeds listDataCode (dataToBuiltinData (List [I 1, I 2])),
          testCase "I" $ succeeds integerDataCode (dataToBuiltinData (I (-2))),
          testCase "B" $ succeeds bytesDataCode (dataToBuiltinData (B "\NUL\SI\DLE\255"))
        ],
      testGroup
        "version-specific instances"
        [ testCase "V1" $ succeeds v1Code unitData,
          testCase "V2 output datum constructors" $ succeeds v2Code unitData,
          testCase "V3 votes and script purposes" $ succeeds v3Code unitData
        ]
    ]

unitData :: BuiltinData
unitData = dataToBuiltinData (I 0)

integerCode :: CompiledCode (BuiltinData -> P.BuiltinUnit)
integerCode =
  $$( compile
        [||
        \_ ->
          P.check
            ( showBS (0 :: Integer)
                P.== "0"
                P.&& showBS (1234567890 :: Integer)
                P.== "1234567890"
                P.&& showBS (-1234567890 :: Integer)
                P.== "-1234567890"
            )
        ||]
    )

byteStringCode :: CompiledCode (BuiltinData -> P.BuiltinUnit)
byteStringCode =
  $$( compile
        [||
        \_ ->
          P.check
            ( showBS ("" :: BuiltinByteString)
                P.== "\"\""
                P.&& showBS ("\NUL\SI\DLE\255" :: BuiltinByteString)
                P.== "\"000f10ff\""
            )
        ||]
    )

containerCode :: CompiledCode (BuiltinData -> P.BuiltinUnit)
containerCode =
  $$( compile
        [||
        \_ ->
          P.check
            ( showBS ([] :: [Integer])
                P.== "[]"
                P.&& showBS ([1, -2, 30] :: [Integer])
                P.== "[1,-2,30]"
                P.&& showBS (P.Just True)
                P.== "(Just True)"
                P.&& showBS (P.Nothing :: Maybe Integer)
                P.== "Nothing"
                P.&& showBS ((1, True) :: (Integer, Bool))
                P.== "(1,True)"
                P.&& showBS (AssocMap.singleton (1 :: Integer) True)
                P.== "(fromList [(1,True)])"
            )
        ||]
    )

constrDataCode :: CompiledCode (BuiltinData -> P.BuiltinUnit)
constrDataCode =
  $$( compile
        [||
        \d ->
          P.check (showBS d P.== "(BuiltinData (Constr 0 [(I 1)]))")
        ||]
    )

mapDataCode :: CompiledCode (BuiltinData -> P.BuiltinUnit)
mapDataCode =
  $$( compile
        [||
        \d ->
          P.check (showBS d P.== "(BuiltinData (Map [((I 1),(B \"61\"))]))")
        ||]
    )

listDataCode :: CompiledCode (BuiltinData -> P.BuiltinUnit)
listDataCode =
  $$( compile
        [||
        \d ->
          P.check (showBS d P.== "(BuiltinData (List [(I 1),(I 2)]))")
        ||]
    )

integerDataCode :: CompiledCode (BuiltinData -> P.BuiltinUnit)
integerDataCode =
  $$( compile
        [||
        \d ->
          P.check (showBS d P.== "(BuiltinData (I -2))")
        ||]
    )

bytesDataCode :: CompiledCode (BuiltinData -> P.BuiltinUnit)
bytesDataCode =
  $$( compile
        [||
        \d ->
          P.check (showBS d P.== "(BuiltinData (B \"000f10ff\"))")
        ||]
    )

v1Code :: CompiledCode (BuiltinData -> P.BuiltinUnit)
v1Code =
  $$( compile
        [||
        \_ ->
          P.check
            ( showBS (V1.POSIXTime 42)
                P.== "(POSIXTime 42)"
                P.&& showBS (V1.Minting (V1.CurrencySymbol "ab"))
                P.== "(Minting (CurrencySymbol \"6162\"))"
                P.&& showBS (V1.Certifying V1.DCertGenesis)
                P.== "(Certifying DCertGenesis)"
            )
        ||]
    )

v2Code :: CompiledCode (BuiltinData -> P.BuiltinUnit)
v2Code =
  $$( compile
        [||
        \_ ->
          P.check
            ( showBS V2.NoOutputDatum
                P.== "NoOutputDatum"
                P.&& showBS (V2.OutputDatumHash (V2.DatumHash "ab"))
                P.== "(OutputDatumHash (DatumHash \"6162\"))"
            )
        ||]
    )

v3Code :: CompiledCode (BuiltinData -> P.BuiltinUnit)
v3Code =
  $$( compile
        [||
        \_ ->
          P.check
            ( showBS V3.VoteNo
                P.== "No"
                P.&& showBS V3.VoteYes
                P.== "Yes"
                P.&& showBS V3.Abstain
                P.== "Abstain"
                P.&& showBS V3.DRepAlwaysAbstain
                P.== "DRep always abstain"
                P.&& showBS V3.DRepAlwaysNoConfidence
                P.== "DRep always no confidence"
                P.&& showBS (V3.Minting (V1.CurrencySymbol "ab"))
                P.== "(Minting (CurrencySymbol \"6162\"))"
                P.&& showBS (V3.Voting (V3.StakePoolVoter (V1.PubKeyHash "cd")))
                P.== "(Voting (Stake Pool Voter (PubKeyHash \"6364\")))"
            )
        ||]
    )

succeeds :: CompiledCode (BuiltinData -> P.BuiltinUnit) -> BuiltinData -> IO ()
succeeds code input =
  assertBool "expected ShowBS output to match" (evaluateCode code input)

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
