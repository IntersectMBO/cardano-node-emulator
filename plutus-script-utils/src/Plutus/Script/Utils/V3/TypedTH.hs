{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Template Haskell alternative to 'mkMultiPurposeScript' that generates a
-- @case@ expression containing only the branches for purposes the caller
-- declares active, instead of always compiling in all 6.
module Plutus.Script.Utils.V3.TypedTH
  ( Purpose (..),
    ActivePurposes,
    allPurposes,
    noPurposes,
    mkMultiPurposeScriptFor,
  )
where

import Control.Monad (replicateM)
import Data.Char (toUpper)
import Data.List (nub)
import Language.Haskell.TH
  ( Body (GuardedB, NormalB),
    Exp (CaseE, VarE),
    Guard (PatG),
    Match (Match),
    Name,
    Q,
    Stmt (BindS),
    appE,
    conP,
    litE,
    mkName,
    newName,
    stringL,
    varE,
    varP,
    wildP,
  )
import Plutus.Script.Utils.V3.Typed
  ( ScriptContextResolvedScriptInfo (..),
    TypedMultiPurposeScript (..),
    deserializeContext,
    fromBuiltinDataEither,
  )
import PlutusLedgerApi.V3
  ( Datum (Datum),
    ScriptInfo (CertifyingScript, MintingScript, ProposingScript, RewardingScript, SpendingScript, VotingScript),
  )
import PlutusTx.Prelude (check, trace, traceError)

-- ============================================================
-- PART 1 — What the caller tells us at compile time
-- ============================================================

-- | One of the 6 script purposes.
data Purpose = Certifying | Minting | Proposing | Rewarding | Spending | Voting
  deriving (Eq, Enum, Bounded)

-- | Which purposes a script actually implements, decided at compile time.
--
-- @
-- mintAndSpendOnly :: ActivePurposes
-- mintAndSpendOnly = [Minting, Spending]
-- @
type ActivePurposes = [Purpose]

-- | All six purposes — same behaviour as the original 'mkMultiPurposeScript'.
allPurposes :: ActivePurposes
allPurposes = [minBound .. maxBound]

-- | No purposes. Use as a base: @Minting : Spending : noPurposes@.
noPurposes :: ActivePurposes
noPurposes = []

-- ============================================================
-- PART 2 — Generating one case branch per purpose
-- ============================================================

-- | Build the guard @| Just handlerVar <- purposeField@ as a real 'PatG'
-- pattern guard (a plain @[| Just h <- purposeField |]@ quote won't parse —
-- @<-@ only works inside a @do@ block).
--
-- @purposeField@ is a 'String' turned into a 'Name' via 'mkName', not
-- captured with @'foo@. Each branch here is built in its own 'Q'
-- computation, then spliced into the outer @TypedMultiPurposeScript{..}@
-- lambda in 'mkMultiPurposeScriptFor'. @'foo@ resolves once, at this
-- module's compile time, to the top-level field accessor — 'mkName'
-- instead stays unresolved until the splice site, where lexical scoping
-- picks up the @TypedMultiPurposeScript{..}@-bound local. Using @'foo@ here
-- would silently reintroduce the runtime @Maybe@ check this module exists
-- to eliminate.
purposeGuard :: String -> Name -> Q Guard
purposeGuard purposeField handlerVar = do
  pat <- conP 'Just [varP handlerVar]
  pure $ PatG [BindS pat (VarE (mkName purposeField))]

-- | Build the two branches for a purpose whose constructor is just the
-- redeemer/txInfo plus some fixed number of extra positional fields (the
-- scrutinee, an index, a credential, ...) — every purpose except 'Spending',
-- which also has to resolve an optional datum.
simplePurposeBranches :: Name -> Int -> String -> String -> Q [Match]
simplePurposeBranches ctorName extraArity purposeField label = do
  extraVs <- replicateM extraArity (newName "arg")
  handlerV <- newName "h"
  redV <- newName "red"
  txIV <- newName "txI"

  let handlerApplication = foldl appE (varE handlerV) (map varE extraVs <> [varE redV, varE txIV])
      runningMsg = "Running the validator with the " <> label <> " script purpose"
      unsupportedMsg = "Unsupported purpose: " <> capitalize label

  let activeBranch = do
        pat <- conP ctorName (map varP extraVs)
        guard <- purposeGuard purposeField handlerV
        body <-
          [|
            do
              ($(varP redV), $(varP txIV)) <-
                deserializeContext $(litE (stringL label)) $(varE (mkName "rsiRedeemer")) $(varE (mkName "rsiTxInfo"))
              return $ trace $(litE (stringL runningMsg)) $(handlerApplication)
            |]
        return $ Match pat (GuardedB [(guard, body)]) []

  let errorBranch = do
        pat <- conP ctorName (map (const wildP) extraVs)
        body <- [|traceError $(litE (stringL unsupportedMsg))|]
        return $ Match pat (NormalB body) []

  sequence [activeBranch, errorBranch]

capitalize :: String -> String
capitalize [] = []
capitalize (c : cs) = toUpper c : cs

spendingBranches :: Q [Match]
spendingBranches = do
  oRefV <- newName "oRef"
  mDatV <- newName "mDat"
  shV <- newName "sh"
  redV <- newName "red"
  txIV <- newName "txI"
  mResV <- newName "mRes"
  bdV <- newName "bd"

  let activeBranch = do
        pat <- [p|SpendingScript $(varP oRefV) $(varP mDatV)|]
        guard <- purposeGuard "spendingPurpose" shV
        body <-
          [|
            do
              ($(varP redV), $(varP txIV)) <-
                deserializeContext "spending" $(varE (mkName "rsiRedeemer")) $(varE (mkName "rsiTxInfo"))
              $(varP mResV) <- case $(varE mDatV) of
                Nothing ->
                  return Nothing
                Just (Datum $(varP bdV)) ->
                  Just <$> fromBuiltinDataEither "datum" $(varE bdV)
              return $
                trace "Running the validator with the spending script purpose" $
                  $(varE shV) $(varE oRefV) $(varE mResV) $(varE redV) $(varE txIV)
            |]
        return $ Match pat (GuardedB [(guard, body)]) []

  let errorBranch = do
        pat <- [p|SpendingScript {}|]
        body <- [|traceError "Unsupported purpose: Spending"|]
        return $ Match pat (NormalB body) []

  sequence [activeBranch, errorBranch]

-- ============================================================
-- PART 3 — Assemble everything into one function expression
-- ============================================================

-- | Generate a @mkMultiPurposeScript@-like function containing only the
-- branches for the purposes listed in 'ActivePurposes'.
--
-- = Usage (in your script module)
--
-- @
-- {-\# LANGUAGE TemplateHaskell \#-}
-- import Plutus.Script.Utils.V3.TypedTH
--
-- -- Only Minting + Spending branches will exist in the compiled script.
-- myScript
--   :: TypedMultiPurposeScript () () mintRed mintTxInfo () () () () sDat sRed sTxInfo () ()
--   -> BuiltinData -> BuiltinUnit
-- myScript = $(mkMultiPurposeScriptFor [Minting, Spending])
-- @
-- | With no active purposes, never deserialize anything and always fail.
mkMultiPurposeScriptFor :: ActivePurposes -> Q Exp
mkMultiPurposeScriptFor (nub -> []) = [|\_ _ -> check False|]
mkMultiPurposeScriptFor active = do
  let branchesFor = \case
        Certifying -> simplePurposeBranches 'CertifyingScript 2 "certifyingPurpose" "certifying"
        Minting -> simplePurposeBranches 'MintingScript 1 "mintingPurpose" "minting"
        Proposing -> simplePurposeBranches 'ProposingScript 2 "proposingPurpose" "proposing"
        Rewarding -> simplePurposeBranches 'RewardingScript 1 "rewardingPurpose" "rewarding"
        Spending -> spendingBranches
        Voting -> simplePurposeBranches 'VotingScript 1 "votingPurpose" "voting"

  allBranches <- concat <$> traverse branchesFor active

  [|
    \TypedMultiPurposeScript {..} dat ->
      either traceError check $ do
        ScriptContextResolvedScriptInfo {..} <-
          fromBuiltinDataEither "script info" dat
        $(pure (CaseE (VarE (mkName "rsiScriptInfo")) allBranches))
    |]
