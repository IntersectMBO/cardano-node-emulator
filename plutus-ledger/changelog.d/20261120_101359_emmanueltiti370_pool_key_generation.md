### Added

- `Ledger.CardanoPool`: deterministic mock stake pool key generation for the
  emulator, mirroring `Ledger.CardanoWallet`'s `knownMockWallets` pattern.
  Generates cold (`StakePoolKey`), VRF, and KES signing keys purely from a
  seed, with no `IO` dependency. Adds `MockPool`, `MockPoolKeys`,
  `PoolNumber`, `genPool`, `genPoolKeys`, `knownMockPools`, `knownMockPool`,
  and `poolIdBech32`.
