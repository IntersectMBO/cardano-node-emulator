# `mkMultiPurposeScriptFor`: input/output examples

`mkMultiPurposeScriptFor` reads a `TypedMultiPurposeScript` value's own
`Just`/`Nothing` fields and generates a function containing only the `case`
branches for the purposes that are `Just`. These examples show, for a few
concrete scripts, exactly what gets generated — written out as ordinary
Haskell for readability (the real splice uses fresh, auto-generated variable
names, but this is the same logic).

In every example below:

```haskell
myScript = $(mkMultiPurposeScriptFor [| theScriptValue |])
```

**Important caveat on the "Input" shown below**: `mkMultiPurposeScriptFor`
currently only understands the script written as a literal
`TypedMultiPurposeScript may1 .. may6` constructor application, with each
argument a literal `Just ..` or `Nothing` — it cannot see through a
combinator chain like `falseTypedMultiPurposeScript \`withMintingPurpose\`
h`, because that's a function call, not a value it can read at compile time.
The examples below therefore write the constructor out directly; they are
equivalent in meaning to the combinator-chain style used elsewhere in this
package, just not (yet) something this macro can parse directly from that
form.

## Example 1 — no active purposes

Input:

```haskell
TypedMultiPurposeScript Nothing Nothing Nothing Nothing Nothing Nothing
--                      ^cert   ^mint   ^propose ^reward ^spend  ^vote
```

Generated:

```haskell
\_ -> check False
```

No purpose is active, so the generated function never even looks at its
`BuiltinData` argument — it never deserializes anything, and always fails.
This is a dedicated special case, not a `case` with zero branches.

## Example 2 — one active purpose (minting)

Input:

```haskell
TypedMultiPurposeScript Nothing (Just mintingHandler) Nothing Nothing Nothing Nothing
```

Generated:

```haskell
\dat ->
  let TypedMultiPurposeScript {..} = theScriptValue
   in either traceError check $ do
        ScriptContextResolvedScriptInfo {..} <- fromBuiltinDataEither "script info" dat
        case rsiScriptInfo of
          MintingScript cur | Just h <- mintingPurpose -> do
            (red, txI) <- deserializeContext "minting" rsiRedeemer rsiTxInfo
            return $ trace "Running the validator with the minting script purpose" $ h cur red txI
          MintingScript {} -> traceError "Unsupported purpose: Minting"
```

Only the `MintingScript` patterns exist. If this script were ever invoked for
any other purpose — say `SpendingScript` — there is no matching pattern at
all, so it fails with a non-exhaustive-pattern-match error rather than the
friendlier `"Unsupported purpose: Spending"` message you'd get from
`mkMultiPurposeScript`. That trade-off (no case for a purpose at all, instead
of a case that always errors) is exactly where the size reduction comes from.

## Example 3 — three active purposes (minting, spending, voting)

Input:

```haskell
TypedMultiPurposeScript
  Nothing                   -- certifying
  (Just mintingHandler)     -- minting
  Nothing                   -- proposing
  Nothing                   -- rewarding
  (Just spendingHandler)    -- spending
  (Just votingHandler)      -- voting
```

Generated:

```haskell
\dat ->
  let TypedMultiPurposeScript {..} = theScriptValue
   in either traceError check $ do
        ScriptContextResolvedScriptInfo {..} <- fromBuiltinDataEither "script info" dat
        case rsiScriptInfo of
          MintingScript cur | Just h <- mintingPurpose -> do
            (red, txI) <- deserializeContext "minting" rsiRedeemer rsiTxInfo
            return $ trace "Running the validator with the minting script purpose" $ h cur red txI
          MintingScript {} -> traceError "Unsupported purpose: Minting"
          SpendingScript oRef mDat | Just h <- spendingPurpose -> do
            (red, txI) <- deserializeContext "spending" rsiRedeemer rsiTxInfo
            mRes <- case mDat of
              Nothing -> return Nothing
              Just (Datum bd) -> Just <$> fromBuiltinDataEither "datum" bd
            return $ trace "Running the validator with the spending script purpose" $ h oRef mRes red txI
          SpendingScript {} -> traceError "Unsupported purpose: Spending"
          VotingScript voter | Just h <- votingPurpose -> do
            (red, txI) <- deserializeContext "voting" rsiRedeemer rsiTxInfo
            return $ trace "Running the validator with the voting script purpose" $ h voter red txI
          VotingScript {} -> traceError "Unsupported purpose: Voting"
```

Spending is the only purpose with extra structure: the optional datum is
resolved separately, immediately after the redeemer/`TxInfo` decode, before
the handler is applied. The other two active purposes (minting, voting) follow
the same shape as each other — redeemer/`TxInfo` decode, then call the handler
directly — which is what `simplePurposeBranches` generalises over internally.

## What never appears in the output

Certifying, proposing, and rewarding never appear anywhere in examples 2 or 3
— not as case branches, not as error messages, nothing. Compare this to
`mkMultiPurposeScript`, which always contains all 6 purposes' `case` branches
(each with a pattern-match `Just` guard plus an `"Unsupported purpose: ..."`
fallback), regardless of which ones a given script actually uses.
