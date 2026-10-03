# Copilot Instructions for cardano-node-emulator

## Repository Overview

This is a Haskell project that provides an in-memory blockchain emulator for testing Cardano smart contracts and transactions. The repository is organized as a multi-package Cabal project with five main packages:

- **cardano-node-emulator**: Core emulator providing an in-memory blockchain
- **cardano-node-socket-emulator**: Socket-based wrapper exposing node-to-client protocol over Unix sockets
- **plutus-ledger**: Ledger types and functionality for Plutus scripts and transactions
- **plutus-script-utils**: Utilities for writing and working with Plutus scripts
- **freer-extras**: Extensions to the `simple-freer` effect system

## Development Environment

**Required setup**: Use Nix exclusively to ensure consistent tool versions across all developers.

```bash
# Enter development shell with all dependencies and tools pre-configured
nix develop

# Inside nix develop, you'll see a menu of available commands
```

The development shell provides:
- GHC 9.6.6 (Haskell compiler)
- Cabal 3.14.1.0 (build tool)
- HLint (linter)
- Ormolu (code formatter)
- Hpack (package definition formatter)
- Pre-commit hooks (automatically run on git commit)

**Without Nix** (not recommended): Refer to CONTRIBUTING.md for workarounds, but be aware you may encounter version mismatches.

## Build, Test & Lint Commands

### Building

```bash
# Build entire workspace
cabal build all

# Build a specific package (e.g., cardano-node-emulator)
cabal build cardano-node-emulator

# Build with tests enabled
cabal build all --enable-tests
```

### Testing

```bash
# Run all tests in the workspace
cabal test all --test-show-details=direct

# Run tests for a specific package
cabal test cardano-node-emulator --test-show-details=direct

# Run a single test module
cabal test cardano-node-emulator --test-show-details=direct -- +RTS -N1 -RTS

# Parallel test execution is not supported; tests run sequentially (-j1)
```

### Code Quality

```bash
# Lint code with HLint
hlint .

# Format Haskell code with Ormolu
# This runs automatically via pre-commit hooks on `git commit`
```

### Documentation

```bash
# Generate Haddock documentation for a package
cabal haddock cardano-node-emulator
```

## Key Architecture & Design Patterns

### Multi-Package Structure

Each package is self-contained with its own `package.yaml` (processed by hpack into `.cabal`), dependencies, and tests. Packages can depend on one another but should maintain clear boundaries.

### Dependency Management

Dependencies are pinned to specific versions using `cabal.project`:
- **Hackage index state**: Pinned to a specific timestamp for reproducibility
- **CHaP (Cardano Haskell Packages)**: Additional repository for Cardano-specific packages
- **Source repository packages**: Rarely used; avoid unless necessary (prefer releasing to CHaP)

To update dependencies: Edit the `index-state` in `cabal.project` and run `cabal update`.

### Effect System

The codebase uses `simple-freer` (Freer monad) for effect composition. The `freer-extras` package provides common extensions and utilities for this.

### Testing with Generators

The emulator includes test data generators in `Cardano.Node.Emulator.Generators` for creating realistic test scenarios.

## Code Conventions

### Haskell Style

- **Column limit**: 100 characters
- **Indentation**: 2 spaces
- **Import style**: `ImportQualifiedPost` enabled (imports appear after the module name)
- **Module organization**: Explicit module exports required (`-Wmissing-import-lists`)
- **Type safety**: Enabled `ScopedTypeVariables`, `StandaloneDeriving`, and strict pattern matching checks

### Common Extensions in Use

Packages enable these by default (check individual `.cabal` files):
- `DeriveFoldable`, `DeriveFunctor`, `DeriveGeneric`: Automatic deriving
- `ExplicitForAll`: Explicit type parameters
- `FlexibleContexts`, `GeneralizedNewtypeDeriving`: More expressive type constraints
- `LambdaCase`: Pattern matching in lambda expressions
- `NamedFieldPuns`: Record field punning
- `ImportQualifiedPost`: Qualified imports after module name

### GHC Compiler Flags

All packages compile with:
```
-Wall -Widentities -Wincomplete-record-updates -Wunused-packages
-Wincomplete-uni-patterns -Wnoncanonical-monad-instances -Wredundant-constraints
-Wmissing-import-lists
```

Plugin flags for PlutusTx scripts:
```
-fplugin-opt PlutusTx.Plugin:target-version=1.1.0
-fplugin-opt PlutusTx.Plugin:defer-errors
```

### HLint Rules

See `.hlint.yaml` for exceptions. Notable patterns:
- `Move brackets to avoid $` is ignored (rarely an improvement)
- `Use <$>` is ignored (clarity is preferred)
- `Avoid lambda` is ignored (named parameters aid clarity)

### Code Structure

- **API boundaries**: Core functionality is in `Cardano.Node.Emulator.API` (public interface)
- **Internal modules**: Implementation details use `Internal.` prefix and are not part of the public API
- **Module hierarchy**: Follows the package structure; internal modules are namespaced separately

## Testing Practices

- Tests are located in a `test/` directory within each package
- Use Tasty for test framework (the standard in this codebase)
- Tests require `TMPDIR`/`TMP` environment variables to be set (GitHub Actions uses `runner.temp`)
- Some tests are stateful and sensitive to parallelism; run with `-j1`
- Test output is colored and displayed directly (see `test-show-details: direct` in `cabal.project`)

## Pre-commit Hooks

Configured in `flake.nix`:
- **Ormolu**: Formats Haskell files
- **Nixfmt**: Formats Nix files
- **Hpack**: Validates and can regenerate `.cabal` files from `package.yaml`

These run automatically on commit when inside `nix develop`. To bypass (not recommended): `git commit --no-verify`.

## CI/CD Pipeline

GitHub Actions runs:
1. Dependency resolution with dry-run build plan
2. Cached dependency build
3. Full build of all packages with tests enabled
4. Test execution with direct output
5. Artifact collection and upload

Cache is invalidated via `CABAL_CACHE_VERSION` in `.github/workflows/haskell.yml`.

## File Locations Reference

- **Main source**: `<package>/src/` (Haskell source files)
- **Package definitions**: `<package>/package.yaml` (processed to `.cabal`)
- **Tests**: `<package>/test/`
- **Cabal configuration**: `cabal.project` (workspace), `<package>/<package>.cabal` (per-package)
- **Nix configuration**: `flake.nix`, `nix/` directory
- **Linting**: `.hlint.yaml`
- **CI/CD**: `.github/workflows/`

## Common Tasks

### Adding a new Haskell package

1. Create the package directory: `mkdir my-package`
2. Add `package.yaml` and `src/` structure
3. Add the package to `cabal.project` under `packages:`
4. Run `cabal build my-package --enable-tests` to verify
5. CI will validate the Nix build

### Updating GHC version

1. Edit the GHC version in `nix/project.nix` (IOGX config)
2. Restart `nix develop` shell
3. Compile packages and fix any compilation errors
4. Update CI workflow if needed (currently 9.6.6)

### Managing dependencies

1. Edit the `package.yaml` for your package
2. If you need a newer version available only after the current `index-state`, bump the timestamp in `cabal.project`
3. Run `cabal update` and `cabal build` to verify the plan works
4. If breakage occurs, add upper/lower bounds to fix it

## Useful Resources

- **Cardano Engineering Handbook**: https://input-output-hk.github.io/cardano-engineering-handbook
- **Contributing Guide**: See CONTRIBUTING.md for detailed development instructions
- **Nix Setup**: Refer to https://github.com/input-output-hk/iogx/blob/main/doc/nix-setup-guide.md
- **Cardano Haskell Packages**: https://github.com/input-output-hk/cardano-haskell-packages
