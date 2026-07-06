
<a id='changelog-1.2.0'></a>
# 1.2.0 — 2023-03-03

## Removed

- Removed `OpenApi.ToSchema` instance for `SlotConfig`.

- Remove `estimateTransactionFee`, `signTx`, `fromPlutusTx`, `fromPlutusTxSigned`, `fromPlutusTxSigned'` as the `Tx` was removed from `plutus-ledger`.

## Changed

- The default utxo provider for balancing now selects bigger inputs first when adding new inputs, to reduce the number of inputs.
  This was in particular a problem for collateral inputs, of which there can only be 3.

- **Security**: emulator now rejects transactions containing duplicate inputs.
  This closes a red‑team finding (double-spend/DoS) by performing an early
  validation step before applying the transaction to the ledger.
  Added corresponding unit tests in `ValidationSpec.hs`.

<a id='changelog-1.1.0'></a>
# 1.1.0 — 2023-01-12

## Added

- Moved from `plutus-ledger` package:
  - `Ledger.TimeSlot` to `Cardano.Node.Emulator.TimeSlot`
  - `Ledger.Params` to `Cardano.Node.Emulator.Params`
  - `Ledger.Generators` to `Cardano.Node.Emulator.Generators`
  - `Ledger.Fee` to `Cardano.Node.Emulator.Fee`
  - `Ledger.Validation` to `Cardano.Node.Emulator.Validation`
  - `Wallet.Emulator.Chain` to `Cardano.Node.Emulator.Chain`
