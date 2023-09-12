# This file is part of the IOGX template and is documented at the link below:
# https://www.github.com/input-output-hk/iogx#33-nixcabal-projectnix

{ inputs, inputs', meta, config, pkgs, l, ... }:

let

  # Only a limited subset of components can be cross-compiled on windows.
  # When `isCross` is `true`, it means that we are cross-compiling the project.
  isCross = pkgs.stdenv.hostPlatform != pkgs.stdenv.buildPlatform;

  sha256map = {
    "https://github.com/input-output-hk/cardano-node"."a158a679690ed8b003ee06e1216ac8acd5ab823d" = "sha256-uY7wPyCgKuIZcGu0+vGacjGw2kox8H5ZsVGsfTNtU0c=";
  };

  packages = {
    cardano-node-emulator.doHaddock = meta.enableHaddock;
    cardano-node-emulator.flags.defer-plugin-errors = meta.enableHaddock;

    # Werror everything. This is a pain, see https://github.com/input-output-hk/haskell.nix/issues/519
    cardano-node-emulator.ghcOptions = [ "-Werror" ];
  };


  modules = [{ inherit packages; }];


  project = { inherit sha256map modules; };

in

project
