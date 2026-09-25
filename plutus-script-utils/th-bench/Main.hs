{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- | Compiles the same minting+spending validator two ways — via the
-- hand-written 'mkMultiPurposeScript' (which always contains all 6 purpose
-- branches) and via the Template Haskell 'mkMultiPurposeScriptFor' (which
-- contains only the 2 requested branches) — and prints the size difference.
--
-- Run with: cabal run plutus-script-utils-th-bench
module Main (main) where

import Plutus.Script.Utils.V3.Generators
  ( falseTypedMultiPurposeScript,
    trueMintingPurpose,
    trueSpendingPurpose,
    withMintingPurpose,
    withSpendingPurpose,
  )
import Plutus.Script.Utils.V3.Typed (TypedMultiPurposeScript, mkMultiPurposeScript)
import Plutus.Script.Utils.V3.TypedTH (ActivePurposes (..), mkMultiPurposeScriptFor, noPurposes)
import PlutusLedgerApi.V3 (BuiltinData)
import PlutusTx.Code (sizePlc)
import PlutusTx.Prelude (BuiltinUnit)
import PlutusTx.TH (compile)

-- | TH-generated equivalent of 'mkMultiPurposeScript', restricted to
-- minting + spending (4 of the possible 12 branches).
--
-- The 'ActivePurposes' value must stay inline in the splice rather than a
-- top-level binding — GHC's stage restriction forbids a splice referencing
-- a same-module top-level name.
{-# INLINEABLE thMkMultiPurposeScript #-}
thMkMultiPurposeScript ::
  TypedMultiPurposeScript () () () () () () () () () () () () () -> BuiltinData -> BuiltinUnit
thMkMultiPurposeScript =
  ( $(mkMultiPurposeScriptFor (noPurposes {hasMinting = True, hasSpending = True})) ::
      TypedMultiPurposeScript () () () () () () () () () () () () () -> BuiltinData -> BuiltinUnit
  )

-- | Same script, fed into both compilers below.
script :: TypedMultiPurposeScript () () () () () () () () () () () () ()
script =
  falseTypedMultiPurposeScript
    `withMintingPurpose` trueMintingPurpose @() @()
    `withSpendingPurpose` trueSpendingPurpose @() @() @()

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
