### Removed

- Removed `TypedScriptTxOut`, `TypedScriptTxOutRef`, `makeTypedScriptTxOut`,
  `typeScriptTxOut`, `typeScriptTxOutRef`, `checkDatum`,
  `checkMultiPurposeScriptAddress`, `WrongOutTypeError`,
  `MultiPurposeScriptTypes`, `IsDataDatum`, `generalise`, `Any`,
  `toCardanoAddressAny` and the primed `*PurposeType'` synonyms from
  `Plutus.Script.Utils.V3.Typed`. They are outdated and will no longer be part
  of the project's content.

### Changed

- Reworked `Plutus.Script.Utils.V3.Typed` and
  `Plutus.Script.Utils.V3.Generators` so that a multi-purpose script is
  parameterised directly by its per-purpose handler types
  (`CertifyingPurposeType`, `MintingPurposeType`, `ProposingPurposeType`,
  `RewardingPurposeType`, `SpendingPurposeType`, `VotingPurposeType`) instead of
  a type-family-indexed record, which the Plutus compiler could not process.
- The on-chain `outputsAt` helpers in `Plutus.Script.Utils.V1.Contexts`,
  `Plutus.Script.Utils.V2.Contexts` and `Plutus.Script.Utils.V3.Contexts` no
  longer use list comprehensions (they use `mapMaybe`), so they compile reliably
  under the PlutusTx plugin.

### Added

- Added a test suite for the V3 typed multi-purpose script dispatcher and its
  forwarding generators.
