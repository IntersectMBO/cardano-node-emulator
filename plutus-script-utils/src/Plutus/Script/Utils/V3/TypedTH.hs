{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Template Haskell alternative to 'mkMultiPurposeScript' that generates a
-- @case@ expression containing only the branches for purposes the given
-- script actually implements, inferred from its own @Just@/@Nothing@
-- fields instead of asked for separately.
module Plutus.Script.Utils.V3.TypedTH
  ( Purpose (..),
    mkMultiPurposeScriptFor,
  )
where

import Control.Monad (replicateM)
import Data.Char (toUpper)
import Language.Haskell.TH
  ( Body (GuardedB, NormalB),
    Exp (AppE, CaseE, ConE, InfixE, SigE, VarE),
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
    addCertifyingPurpose,
    addMintingPurpose,
    addProposingPurpose,
    addRewardingPurpose,
    addSpendingPurpose,
    addVotingPurpose,
    deserializeContext,
    falseTypedMultiPurposeScript,
    fromBuiltinDataEither,
    trueTypedMultiPurposeScript,
    withCertifyingPurpose,
    withMintingPurpose,
    withProposingPurpose,
    withRewardingPurpose,
    withSpendingPurpose,
    withVotingPurpose,
  )
import PlutusLedgerApi.V3
  ( Datum (Datum),
    ScriptInfo (CertifyingScript, MintingScript, ProposingScript, RewardingScript, SpendingScript, VotingScript),
  )
import PlutusTx.Prelude (check, trace, traceError)

-- ============================================================
-- PART 1 — Reading which purposes are active off the script itself
-- ============================================================

-- | One of the 6 script purposes.
data Purpose = Certifying | Minting | Proposing | Rewarding | Spending | Voting
  deriving (Eq, Enum, Bounded)

-- | The 6 purpose fields of a 'TypedMultiPurposeScript', in constructor order.
purposesInOrder :: [Purpose]
purposesInOrder = [Certifying, Minting, Proposing, Rewarding, Spending, Voting]

-- | Which purposes @scriptExp@ ends up with set to @Just _@, read directly
-- off its source, without running it. Two shapes are understood:
--
--   * A literal @TypedMultiPurposeScript may1 .. may6@ application, each
--     @mayN@ a literal @Just ..@ or @Nothing@.
--
--   * 'falseTypedMultiPurposeScript' or 'trueTypedMultiPurposeScript',
--     optionally chained through any number of
--     @withXPurpose@\/@addXPurpose@ calls, e.g.
--     @falseTypedMultiPurposeScript \`withMintingPurpose\` h@ — the usual
--     way scripts are built in this package.
--
-- Anything else (a variable holding some other pre-built script, a call to
-- something that isn't one of the combinators above) can't be read without
-- actually running the program, which isn't available at this stage.
activePurposesOf :: Exp -> Q [Purpose]
activePurposesOf = go . stripSig
  where
    stripSig (SigE e _) = e
    stripSig e = e

    go expr
      | Just args <- literalCtorArgs expr =
          pure [p | (p, arg) <- zip purposesInOrder args, isJustE arg]
    go (AppE (AppE (VarE combName) baseExpr) _handler)
      | Just purpose <- combinatorPurpose combName = do
          rest <- go (stripSig baseExpr)
          pure (if purpose `elem` rest then rest else purpose : rest)
    -- Backtick infix application (`baseExpr \`combName\` handler`) is its
    -- own 'InfixE' node, not nested 'AppE' — distinct from the prefix form
    -- above even though they mean the same thing.
    go (InfixE (Just baseExpr) (VarE combName) (Just _handler))
      | Just purpose <- combinatorPurpose combName = do
          rest <- go (stripSig baseExpr)
          pure (if purpose `elem` rest then rest else purpose : rest)
    go (VarE n)
      | n == 'falseTypedMultiPurposeScript = pure []
      | n == 'trueTypedMultiPurposeScript = pure purposesInOrder
    go _ =
      fail
        "mkMultiPurposeScriptFor: could not read which purposes are active \
        \from this expression. Supported shapes: a literal \
        \`TypedMultiPurposeScript may1 .. may6` application; or \
        \`falseTypedMultiPurposeScript`/`trueTypedMultiPurposeScript`, \
        \optionally chained through `withXPurpose`/`addXPurpose` calls."

    literalCtorArgs :: Exp -> Maybe [Exp]
    literalCtorArgs = collect []
      where
        collect acc (AppE f x) = collect (x : acc) f
        collect acc (ConE n) | n == 'TypedMultiPurposeScript, length acc == 6 = Just acc
        collect _ _ = Nothing

    isJustE (AppE (ConE n) _) | n == 'Just = True
    isJustE (ConE n) | n == 'Nothing = False
    isJustE _ =
      error
        "mkMultiPurposeScriptFor: every field of the script must be a \
        \literal `Just ..` or `Nothing`, not a variable or other \
        \expression, so the active purposes can be read at compile time"

    combinatorPurpose :: Name -> Maybe Purpose
    combinatorPurpose n
      | n `oneOf` ['withCertifyingPurpose, 'addCertifyingPurpose] = Just Certifying
      | n `oneOf` ['withMintingPurpose, 'addMintingPurpose] = Just Minting
      | n `oneOf` ['withProposingPurpose, 'addProposingPurpose] = Just Proposing
      | n `oneOf` ['withRewardingPurpose, 'addRewardingPurpose] = Just Rewarding
      | n `oneOf` ['withSpendingPurpose, 'addSpendingPurpose] = Just Spending
      | n `oneOf` ['withVotingPurpose, 'addVotingPurpose] = Just Voting
      | otherwise = Nothing
      where
        oneOf x xs = x `elem` xs

-- ============================================================
-- PART 2 — Generating one case branch per purpose
-- ============================================================

-- | Guard @| Just handlerVar <- purposeField@, built as 'PatG' since
-- @[| Just h <- purposeField |]@ doesn't parse (@<-@ needs a @do@ block).
--
-- @purposeField@ becomes a 'Name' via 'mkName', not @'foo@. This guard is
-- spliced into a lambda built elsewhere, in 'mkMultiPurposeScriptFor';
-- @'foo@ would resolve now, to the top-level accessor in "Typed", not to
-- that lambda's local binding — silently reintroducing the runtime @Maybe@
-- check this module exists to remove.
purposeGuard :: String -> Name -> Q Guard
purposeGuard purposeField handlerVar = do
  pat <- conP 'Just [varP handlerVar]
  pure $ PatG [BindS pat (VarE (mkName purposeField))]

-- | Two branches (handler present / missing) for a purpose whose
-- 'ScriptInfo' constructor is fixed positional fields plus redeemer and
-- @TxInfo@. Every purpose but 'Spending' (optional datum, handled below).
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

-- | Upper-case the first letter, e.g. @"spending"@ to @"Spending"@.
capitalize :: String -> String
capitalize [] = []
capitalize (c : cs) = toUpper c : cs

-- | 'Spending'\'s two branches: the datum is optional, decoded separately
-- when present, unlike 'simplePurposeBranches'.
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

-- | Generate a @mkMultiPurposeScript@-like function, specialised to
-- @scriptExp@, containing only the branches for the purposes it actually
-- implements — read off its @Just@/@Nothing@ fields, not asked for
-- separately. With no active purpose, the generated function never
-- deserializes and always fails.
--
-- @scriptExp@ must be written directly inside the quote at the splice
-- site: either a literal @TypedMultiPurposeScript ..@ application, or
-- 'falseTypedMultiPurposeScript'\/'trueTypedMultiPurposeScript' chained
-- through @withXPurpose@\/@addXPurpose@ calls — see 'activePurposesOf' for
-- the exact shapes understood. Quoting a reference to some other top-level
-- binding, e.g. @[| myScript |]@, only captures that reference, not the
-- definition it points to, so there is nothing here for this function to
-- read.
--
-- Any purpose left @Nothing@ leaves its redeemer\/@TxInfo@ type variables
-- completely unused, so nothing pins them — a type annotation inside the
-- quote (as below) is required whenever at least one purpose is inactive.
--
-- = Usage (in your script module)
--
-- @
-- {-\# LANGUAGE TemplateHaskell \#-}
-- import Plutus.Script.Utils.V3.Typed
-- import Plutus.Script.Utils.V3.TypedTH
--
-- -- Every unused slot is 'BuiltinData', not @()@: @()@ only decodes
-- -- successfully against a specific empty structure and would reject real
-- -- transaction data, whereas 'BuiltinData' never fails to decode.
-- validate
--   :: BuiltinData -> BuiltinUnit
-- validate =
--   $(mkMultiPurposeScriptFor
--       [| falseTypedMultiPurposeScript
--            `withMintingPurpose` mintingHandler
--            `withSpendingPurpose` spendingHandler
--            :: TypedMultiPurposeScript
--                 BuiltinData BuiltinData mintRed mintTxInfo
--                 BuiltinData BuiltinData BuiltinData BuiltinData
--                 sDat sRed sTxInfo BuiltinData BuiltinData |])
-- @
mkMultiPurposeScriptFor :: Q Exp -> Q Exp
mkMultiPurposeScriptFor scriptExpQ = do
  scriptExp <- scriptExpQ
  active <- activePurposesOf scriptExp
  case active of
    [] -> [|\_ -> check False|]
    _ -> do
      let branchesFor = \case
            Certifying -> simplePurposeBranches 'CertifyingScript 2 "certifyingPurpose" "certifying"
            Minting -> simplePurposeBranches 'MintingScript 1 "mintingPurpose" "minting"
            Proposing -> simplePurposeBranches 'ProposingScript 2 "proposingPurpose" "proposing"
            Rewarding -> simplePurposeBranches 'RewardingScript 1 "rewardingPurpose" "rewarding"
            Spending -> spendingBranches
            Voting -> simplePurposeBranches 'VotingScript 1 "votingPurpose" "voting"

      allBranches <- concat <$> traverse branchesFor active

      [|
        \dat ->
          let TypedMultiPurposeScript {..} = $(pure scriptExp)
           in either traceError check $ do
                ScriptContextResolvedScriptInfo {..} <-
                  fromBuiltinDataEither "script info" dat
                $(pure (CaseE (VarE (mkName "rsiScriptInfo")) allBranches))
        |]
