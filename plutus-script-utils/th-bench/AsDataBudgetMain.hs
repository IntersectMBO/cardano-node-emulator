{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Follow-up to AsDataMain.hs: measures real execution budget (CPU/memory
-- units), not just static script size, using 'evaluateScriptCounting'
-- against the same serialised sample 'ScriptContext' for both scripts.
--
-- Run with: cabal run plutus-script-utils-th-bench-asdata-budget
module Main (main) where

import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Writer (runWriter)
import PlutusLedgerApi.Common qualified as Common
import PlutusLedgerApi.Data.V3 qualified as Lazy
import PlutusLedgerApi.Test.V3.EvaluationContext (costModelParamsForTesting)
import PlutusLedgerApi.V3 qualified as Eager
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Code (CompiledCode, sizePlc)
import PlutusTx.Data.List qualified as DList
import PlutusTx.List qualified as List
import PlutusTx.Prelude
import PlutusTx.TH (compile)
import Prelude qualified as Haskell

-- Same validators as AsDataMain.hs.

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

-- Sample ScriptContext, built with the eager ADTs and reused for both
-- evaluations — asData's types share the same 'Data' encoding by design.

sampleTxInfo :: Eager.TxInfo
sampleTxInfo =
  Eager.TxInfo
    { Eager.txInfoInputs = [],
      Eager.txInfoReferenceInputs = [],
      Eager.txInfoOutputs = [],
      Eager.txInfoFee = Eager.Lovelace 0,
      Eager.txInfoMint = Eager.emptyMintValue,
      Eager.txInfoTxCerts = [],
      Eager.txInfoWdrl = AssocMap.empty,
      Eager.txInfoValidRange = Eager.always,
      Eager.txInfoSignatories = [Eager.PubKeyHash "deadbeef"],
      Eager.txInfoRedeemers = AssocMap.empty,
      Eager.txInfoData = AssocMap.empty,
      Eager.txInfoId = Eager.TxId "0000000000000000000000000000000000000000000000000000000000000000",
      Eager.txInfoVotes = AssocMap.empty,
      Eager.txInfoProposalProcedures = [],
      Eager.txInfoCurrentTreasuryAmount = Nothing,
      Eager.txInfoTreasuryDonation = Nothing
    }

sampleScriptContext :: Eager.ScriptContext
sampleScriptContext =
  Eager.ScriptContext
    { Eager.scriptContextTxInfo = sampleTxInfo,
      Eager.scriptContextRedeemer = Eager.Redeemer (toBuiltinData ()),
      Eager.scriptContextScriptInfo = Eager.MintingScript (Eager.CurrencySymbol "")
    }

sampleArg :: Common.Data
sampleArg = Common.builtinDataToData (toBuiltinData sampleScriptContext)

protocolVersion :: Common.MajorProtocolVersion
protocolVersion = Common.MajorProtocolVersion 9 -- Chang HF; protocol version PlutusV3 was introduced in

evalCtx :: Common.EvaluationContext
evalCtx =
  case runWriter (runExceptT (Eager.mkEvaluationContext (Haskell.map Haskell.snd costModelParamsForTesting))) of
    (Haskell.Left err, _warnings) -> Haskell.error ("could not build evaluation context: " Haskell.++ Haskell.show err)
    (Haskell.Right ctx, _warnings) -> ctx

exBudgetOf :: CompiledCode (BuiltinData -> BuiltinUnit) -> Haskell.IO Common.ExBudget
exBudgetOf code = do
  script <- case Common.deserialiseScript Common.PlutusV3 protocolVersion (Common.serialiseCompiledCode code) of
    Haskell.Left err -> Haskell.error ("could not deserialise script: " Haskell.++ Haskell.show err)
    Haskell.Right s -> Haskell.pure s
  let (_logOutput, result) = Common.evaluateScriptCounting Common.PlutusV3 protocolVersion Common.Verbose evalCtx script [sampleArg]
  case result of
    Haskell.Left err -> Haskell.error ("evaluation failed: " Haskell.++ Haskell.show err)
    Haskell.Right budget -> Haskell.pure budget

main :: Haskell.IO ()
main = do
  eagerBudget <- exBudgetOf eagerCompiled
  lazyBudget <- exBudgetOf lazyCompiled

  Haskell.putStrLn "=== execution budget: eager (PlutusLedgerApi.V3) vs lazy (PlutusLedgerApi.Data.V3 / asData) ==="
  Haskell.putStrLn (Haskell.mconcat ["eager: ", Haskell.show eagerBudget])
  Haskell.putStrLn (Haskell.mconcat ["lazy:  ", Haskell.show lazyBudget])
  Haskell.putStrLn (Haskell.mconcat ["(for reference, static PLC size — eager: ", Haskell.show (sizePlc eagerCompiled), " nodes, lazy: ", Haskell.show (sizePlc lazyCompiled), " nodes)"])
