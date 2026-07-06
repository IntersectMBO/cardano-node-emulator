# [Cardano Node Emulator](https://github.com/input-output-hk/cardano-node-emulator)

[![Cardano Engineering Handbook](https://img.shields.io/badge/policy-Cardano%20Engineering%20Handbook-informational)](https://input-output-hk.github.io/cardano-engineering-handbook)

## Introduction

TODO

## Architecture

The repository consists of a set of components:

- [cardano-node-emulator](./cardano-node-emulator) - an emulator of the Cardano node, providing an in-memory blockchain for testing.
- [cardano-node-socket-emulator](./cardano-node-socket-emulator) - a socket-based emulator exposing the node-to-client protocol over a Unix socket.
- [plutus-ledger](./plutus-ledger) - ledger types and functionality for working with Plutus scripts and transactions.
- [plutus-script-utils](./plutus-script-utils) - utilities for writing and working with Plutus scripts.
- [freer-extras](./freer-extras) - useful extensions to the `simple-freer` effect system.

## Development

### How to develop and contribute to the project

Run `nix develop` to enter the development shell and you will be presented with a list of available commands.

**Please see [CONTRIBUTING](CONTRIBUTING.md) for comprehensive documentation on how to contribute to the project, including development and submitting changes**

## Documentation

### User documentation

User documentations are work in progress. You may generate Haskell API documentation (haddocks) directly from `nix develop` for each of the components:

```
cabal haddock cardano-node-emulator
```

## Working with the project

### How to submit an issue

Issues can be filed in the [GitHub Issue tracker](https://github.com/input-output-hk/cardano-node-emulator/issues).

However, note that this is pre-release software, so we will not usually be providing support.

### How to develop and contribute to the project

See [CONTRIBUTING](CONTRIBUTING.md), which describes our processes in more detail including development environments; and [Architecture](#architecture), which describes the structure of the repository.

## Licensing

You are free to copy, modify, and distribute the Cardano Node Emulator Library under the terms of the Apache 2.0 license. See the [LICENSE](./LICENSE) and [NOTICE](./NOTICE) files for details.
