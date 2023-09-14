# This file is part of the IOGX template and is documented at the link below:
# https://www.github.com/input-output-hk/iogx#33-nixcabal-projectnix

{ inputs, inputs', meta, config, pkgs, lib, ... }:

let

  # Only a limited subset of components can be cross-compiled on windows.
  # When `isCross` is `true`, it means that we are cross-compiling the project.
  isCross = pkgs.stdenv.hostPlatform != pkgs.stdenv.buildPlatform;

  sha256map = {
    "https://github.com/input-output-hk/cardano-node"."a158a679690ed8b003ee06e1216ac8acd5ab823d" = "sha256-uY7wPyCgKuIZcGu0+vGacjGw2kox8H5ZsVGsfTNtU0c=";
  };

  packages = {
    cardano-node-emulator.doHaddock = meta.enableHaddock;
    cardano-node-socket-emulator.doHaddock = meta.enableHaddock;
    freer-extras.doHaddock = meta.enableHaddock;
    plutus-ledger.doHaddock = meta.enableHaddock;
    plutus-script-utils.doHaddock = meta.enableHaddock;

    cardano-node-emulator.flags.defer-plugin-errors = meta.enableHaddock;
    cardano-node-socket-emulator.flags.defer-plugin-errors = meta.enableHaddock;
    freer-extras.flags.defer-plugin-errors = meta.enableHaddock;
    plutus-ledger.flags.defer-plugin-errors = meta.enableHaddock;
    plutus-script-utils.flags.defer-plugin-errors = meta.enableHaddock;

    # Werror everything. This is a pain, see https://github.com/input-output-hk/haskell.nix/issues/519
    cardano-node-emulator.ghcOptions = [ "-Werror" ];
    cardano-node-socket-emulator.ghcOptions = [ "-Werror" ];
    freer-extras.ghcOptions = [ "-Werror" ];
    plutus-ledger.ghcOptions = [ "-Werror" ];
    plutus-script-utils.ghcOptions = [ "-Werror" ];
  };


  modules = [{ inherit packages; }];


  project = { inherit sha256map modules; };

in

project
