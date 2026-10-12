{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- | Compares 'mkMultiPurposeScript' (all 6 branches) against
-- 'mkMultiPurposeScriptFor' (pruned to 2) on a validator that decodes a real 'TxInfo'.
--
-- Run with: cabal run plutus-script-utils-th-bench
module Main (main) where

import Plutus.Script.Utils.V3.Generators (trueMintingPurpose, trueSpendingPurpose)
import Plutus.Script.Utils.V3.Typed (TypedMultiPurposeScript (TypedMultiPurposeScript), mkMultiPurposeScript)
import Plutus.Script.Utils.V3.TypedTH (mkMultiPurposeScriptFor)
import PlutusLedgerApi.V3 (BuiltinData, TxInfo)
import PlutusTx.Code (sizePlc)
import PlutusTx.Prelude (BuiltinUnit)
import PlutusTx.TH (compile)

-- | 'mkMultiPurposeScriptFor' reads which purposes are active directly off
-- a literal @TypedMultiPurposeScript ..@ expression, so there's no
-- separate purpose list to pass or keep in sync — this is the same
-- minting + spending configuration as 'script' below, written out again
-- because quoting a reference to 'script' would only capture the
-- reference, not the construction this needs to read.
{-# INLINEABLE thMkMultiPurposeScript #-}
thMkMultiPurposeScript ::
  BuiltinData -> BuiltinUnit
thMkMultiPurposeScript =
  $( mkMultiPurposeScriptFor
       [|
         ( TypedMultiPurposeScript
             Nothing
             (Just (trueMintingPurpose @BuiltinData @TxInfo))
             Nothing
             Nothing
             (Just (trueSpendingPurpose @BuiltinData @BuiltinData @TxInfo))
             Nothing ::
             TypedMultiPurposeScript BuiltinData BuiltinData BuiltinData TxInfo BuiltinData BuiltinData BuiltinData BuiltinData BuiltinData BuiltinData TxInfo BuiltinData BuiltinData
         )
         |]
   )

-- | Minting/spending decode a real 'TxInfo'.
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

thCompiled = $$(compile [||thMkMultiPurposeScript||])

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
