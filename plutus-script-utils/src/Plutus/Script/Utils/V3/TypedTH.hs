{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Template Haskell alternative to 'mkMultiPurposeScript' that generates a
-- @case@ expression containing only the branches for purposes the caller
-- declares active, instead of always compiling in all 6.
module Plutus.Script.Utils.V3.TypedTH
  ( ActivePurposes (..),
    allPurposes,
    noPurposes,
    mkMultiPurposeScriptFor,
  )
where

import Language.Haskell.TH
  ( Body (GuardedB, NormalB),
    Exp (CaseE, VarE),
    Guard (PatG),
    Match (Match),
    Name,
    Q,
    Stmt (BindS),
    conP,
    mkName,
    newName,
    varE,
    varP,
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

-- | Which of the 6 purposes does this script actually implement?
--
-- Fill this in at the call site:
--
-- @
-- mintAndSpendOnly :: ActivePurposes
-- mintAndSpendOnly = noPurposes { hasMinting = True, hasSpending = True }
-- @
--
-- GHC evaluates this record at *compile time* before generating any bytecode.
data ActivePurposes = ActivePurposes
  { hasCertifying :: Bool,
    hasMinting :: Bool,
    hasProposing :: Bool,
    hasRewarding :: Bool,
    hasSpending :: Bool,
    hasVoting :: Bool
  }

-- | All six purposes active — same behaviour as the original 'mkMultiPurposeScript'.
allPurposes :: ActivePurposes
allPurposes = ActivePurposes True True True True True True

-- | No purposes active. Use as a base, enabling only what you need.
noPurposes :: ActivePurposes
noPurposes = ActivePurposes False False False False False False

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

mintingBranches :: Q [Match]
mintingBranches = do
  curV <- newName "cur"
  mhV <- newName "mh"
  redV <- newName "red"
  txIV <- newName "txI"

  let activeBranch = do
        pat <- [p|MintingScript $(varP curV)|]
        guard <- purposeGuard "mintingPurpose" mhV
        body <-
          [|
            do
              ($(varP redV), $(varP txIV)) <-
                deserializeContext "minting" $(varE (mkName "rsiRedeemer")) $(varE (mkName "rsiTxInfo"))
              return $
                trace "Running the validator with the minting script purpose" $
                  $(varE mhV) $(varE curV) $(varE redV) $(varE txIV)
            |]
        return $ Match pat (GuardedB [(guard, body)]) []

  let errorBranch = do
        pat <- [p|MintingScript {}|]
        body <- [|traceError "Unsupported purpose: Minting"|]
        return $ Match pat (NormalB body) []

  sequence [activeBranch, errorBranch]

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

certifyingBranches :: Q [Match]
certifyingBranches = do
  iV <- newName "i"
  certV <- newName "cert"
  chV <- newName "ch"
  redV <- newName "red"
  txIV <- newName "txI"

  let activeBranch = do
        pat <- [p|CertifyingScript $(varP iV) $(varP certV)|]
        guard <- purposeGuard "certifyingPurpose" chV
        body <-
          [|
            do
              ($(varP redV), $(varP txIV)) <-
                deserializeContext "certifying" $(varE (mkName "rsiRedeemer")) $(varE (mkName "rsiTxInfo"))
              return $
                trace "Running the validator with the certifying script purpose" $
                  $(varE chV) $(varE iV) $(varE certV) $(varE redV) $(varE txIV)
            |]
        return $ Match pat (GuardedB [(guard, body)]) []

  let errorBranch = do
        pat <- [p|CertifyingScript {}|]
        body <- [|traceError "Unsupported purpose: Certifying"|]
        return $ Match pat (NormalB body) []

  sequence [activeBranch, errorBranch]

proposingBranches :: Q [Match]
proposingBranches = do
  iV <- newName "i"
  propV <- newName "prop"
  phV <- newName "ph"
  redV <- newName "red"
  txIV <- newName "txI"

  let activeBranch = do
        pat <- [p|ProposingScript $(varP iV) $(varP propV)|]
        guard <- purposeGuard "proposingPurpose" phV
        body <-
          [|
            do
              ($(varP redV), $(varP txIV)) <-
                deserializeContext "proposing" $(varE (mkName "rsiRedeemer")) $(varE (mkName "rsiTxInfo"))
              return $
                trace "Running the validator with the proposing script purpose" $
                  $(varE phV) $(varE iV) $(varE propV) $(varE redV) $(varE txIV)
            |]
        return $ Match pat (GuardedB [(guard, body)]) []

  let errorBranch = do
        pat <- [p|ProposingScript {}|]
        body <- [|traceError "Unsupported purpose: Proposing"|]
        return $ Match pat (NormalB body) []

  sequence [activeBranch, errorBranch]

rewardingBranches :: Q [Match]
rewardingBranches = do
  credV <- newName "cred"
  rhV <- newName "rh"
  redV <- newName "red"
  txIV <- newName "txI"

  let activeBranch = do
        pat <- [p|RewardingScript $(varP credV)|]
        guard <- purposeGuard "rewardingPurpose" rhV
        body <-
          [|
            do
              ($(varP redV), $(varP txIV)) <-
                deserializeContext "rewarding" $(varE (mkName "rsiRedeemer")) $(varE (mkName "rsiTxInfo"))
              return $
                trace "Running the validator with the rewarding script purpose" $
                  $(varE rhV) $(varE credV) $(varE redV) $(varE txIV)
            |]
        return $ Match pat (GuardedB [(guard, body)]) []

  let errorBranch = do
        pat <- [p|RewardingScript {}|]
        body <- [|traceError "Unsupported purpose: Rewarding"|]
        return $ Match pat (NormalB body) []

  sequence [activeBranch, errorBranch]

votingBranches :: Q [Match]
votingBranches = do
  voterV <- newName "voter"
  vhV <- newName "vh"
  redV <- newName "red"
  txIV <- newName "txI"

  let activeBranch = do
        pat <- [p|VotingScript $(varP voterV)|]
        guard <- purposeGuard "votingPurpose" vhV
        body <-
          [|
            do
              ($(varP redV), $(varP txIV)) <-
                deserializeContext "voting" $(varE (mkName "rsiRedeemer")) $(varE (mkName "rsiTxInfo"))
              return $
                trace "Running the validator with the voting script purpose" $
                  $(varE vhV) $(varE voterV) $(varE redV) $(varE txIV)
            |]
        return $ Match pat (GuardedB [(guard, body)]) []

  let errorBranch = do
        pat <- [p|VotingScript {}|]
        body <- [|traceError "Unsupported purpose: Voting"|]
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
-- myScript = $(mkMultiPurposeScriptFor (noPurposes { hasMinting = True, hasSpending = True }))
-- @
mkMultiPurposeScriptFor :: ActivePurposes -> Q Exp
mkMultiPurposeScriptFor ActivePurposes {..} = do
  let activeBranchGens :: [Q [Match]]
      activeBranchGens =
        concat
          [ [certifyingBranches | hasCertifying],
            [mintingBranches | hasMinting],
            [proposingBranches | hasProposing],
            [rewardingBranches | hasRewarding],
            [spendingBranches | hasSpending],
            [votingBranches | hasVoting]
          ]

  allBranches <- concat <$> sequence activeBranchGens

  [|
    \TypedMultiPurposeScript {..} dat ->
      either traceError check $ do
        ScriptContextResolvedScriptInfo {..} <-
          fromBuiltinDataEither "script info" dat
        $(pure (CaseE (VarE (mkName "rsiScriptInfo")) allBranches))
    |]
