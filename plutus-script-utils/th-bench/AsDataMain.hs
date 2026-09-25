{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Compares eager vs. lazy field deserialization on one check: "does this
-- transaction have at least one signatory?" Eager decodes the whole
-- 'PlutusLedgerApi.V3.ScriptContext' up front; lazy decodes into the
-- @asData@-backed 'PlutusLedgerApi.Data.V3.ScriptContext', which only
-- deserializes 'txInfoSignatories' and leaves the rest as raw 'BuiltinData'.
--
-- See AsDataBudgetMain.hs for the execution-budget version of this
-- comparison, which is the metric that actually determines fees.
--
-- Run with: cabal run plutus-script-utils-th-bench-asdata
module Main (main) where

import PlutusLedgerApi.Data.V3 qualified as Lazy
import PlutusLedgerApi.V3 qualified as Eager
import PlutusTx.Code (CompiledCode, sizePlc)
import PlutusTx.Data.List qualified as DList
import PlutusTx.List qualified as List
import PlutusTx.Prelude
import PlutusTx.TH (compile)
import Prelude qualified as Haskell

{-# INLINEABLE hasASignatoryEager #-}
hasASignatoryEager :: BuiltinData -> BuiltinUnit
hasASignatoryEager dat =
  case fromBuiltinData dat of
    Nothing -> traceError "bad script context"
    Just Eager.ScriptContext {Eager.scriptContextTxInfo = Eager.TxInfo {Eager.txInfoSignatories}} ->
      check (not (List.null txInfoSignatories))

{-# INLINEABLE hasASignatoryLazy #-}
hasASignatoryLazy :: BuiltinData -> BuiltinUnit
hasASignatoryLazy dat =
  case fromBuiltinData dat of
    Nothing -> traceError "bad script context"
    Just Lazy.ScriptContext {Lazy.scriptContextTxInfo = Lazy.TxInfo {Lazy.txInfoSignatories}} ->
      check (not (DList.null txInfoSignatories))

eagerCompiled :: CompiledCode (BuiltinData -> BuiltinUnit)
eagerCompiled = $$(compile [||hasASignatoryEager||])

lazyCompiled :: CompiledCode (BuiltinData -> BuiltinUnit)
lazyCompiled = $$(compile [||hasASignatoryLazy||])

main :: Haskell.IO ()
main = do
  let eagerSize = sizePlc eagerCompiled
      lazySize = sizePlc lazyCompiled

  Haskell.putStrLn "=== eager (PlutusLedgerApi.V3) vs lazy (PlutusLedgerApi.Data.V3 / asData) ==="
  Haskell.putStrLn (Haskell.mconcat ["PLC AST size (nodes)   eager: ", Haskell.show eagerSize, "   lazy: ", Haskell.show lazySize])
  Haskell.putStrLn
    "NOTE: the metric that actually matters here is execution budget (CPU/\n\
    \memory units at run time), not static size. See the module comment."
