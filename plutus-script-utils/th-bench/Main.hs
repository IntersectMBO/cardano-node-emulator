{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- | Compares 'mkMultiPurposeScript' (all 6 branches) against
-- 'mkMultiPurposeScriptFor' (pruned to 2) on a validator that decodes a real 'TxInfo'.
--
-- Run with: cabal run plutus-script-utils-th-bench
module Main (main) where

import Plutus.Script.Utils.V3.Generators (trueMintingPurpose, trueSpendingPurpose)
import Plutus.Script.Utils.V3.Typed (TypedMultiPurposeScript (TypedMultiPurposeScript), mkMultiPurposeScript)
import Plutus.Script.Utils.V3.TypedTH (Purpose (..), mkMultiPurposeScriptFor)
import PlutusLedgerApi.V3 (BuiltinData, TxInfo)
import PlutusTx.Code (sizePlc)
import PlutusTx.Prelude (BuiltinUnit)
import PlutusTx.TH (compile)

-- | Pruned to minting + spending. Purpose list stays inline — splicing a
-- same-module top-level name is forbidden by GHC's stage restriction.
{-# INLINEABLE thMkMultiPurposeScript #-}
thMkMultiPurposeScript ::
  TypedMultiPurposeScript BuiltinData BuiltinData BuiltinData TxInfo BuiltinData BuiltinData BuiltinData BuiltinData BuiltinData BuiltinData TxInfo BuiltinData BuiltinData -> BuiltinData -> BuiltinUnit
thMkMultiPurposeScript =
  ( $(mkMultiPurposeScriptFor [Minting, Spending]) ::
      TypedMultiPurposeScript BuiltinData BuiltinData BuiltinData TxInfo BuiltinData BuiltinData BuiltinData BuiltinData BuiltinData BuiltinData TxInfo BuiltinData BuiltinData -> BuiltinData -> BuiltinUnit
  )

-- | Minting/spending decode a real 'TxInfo'; every other slot is
-- 'BuiltinData' (never fails to decode), not '()' (fails on real data).
script :: TypedMultiPurposeScript BuiltinData BuiltinData BuiltinData TxInfo BuiltinData BuiltinData BuiltinData BuiltinData BuiltinData BuiltinData TxInfo BuiltinData BuiltinData
script =
  TypedMultiPurposeScript
    Nothing
    (Just (trueMintingPurpose @BuiltinData @TxInfo))
    Nothing
    Nothing
    (Just (trueSpendingPurpose @BuiltinData @BuiltinData @TxInfo))
    Nothing

originalCompiled = $$(compile [||mkMultiPurposeScript script||])

thCompiled = $$(compile [||thMkMultiPurposeScript script||])

main :: IO ()
main = do
  let origSize = sizePlc originalCompiled
      thSize = sizePlc thCompiled

  putStrLn "=== mkMultiPurposeScript (12 branches) vs mkMultiPurposeScriptFor (4 branches) ==="
  putStrLn $ "PLC AST size (nodes)   original: " <> show origSize <> "   TH: " <> show thSize
  putStrLn $
    "Reduction: "
      <> show (100 - (100 * thSize) `div` origSize)
      <> "%"
