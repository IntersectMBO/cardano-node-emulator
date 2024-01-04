{ repoRoot, inputs, pkgs, lib, system }:

let

  cabalProject = pkgs.haskell-nix.cabalProject' {
    name = "cardano-node-emulator";
    src = ../.;
    compiler-nix-name = lib.mkDefault "ghc962";
    flake.variants.ghc928.compiler-nix-name = "ghc928";
    flake.variants.ghc8107.compiler-nix-name = "ghc8107";
    shell.withHoogle = false;
    inputMap = {
      "https://input-output-hk.github.io/cardano-haskell-packages" = inputs.iogx.inputs.CHaP;
    };
    sha256map = {
      "https://github.com/input-output-hk/cardano-node"."a158a679690ed8b003ee06e1216ac8acd5ab823d" = "sha256-uY7wPyCgKuIZcGu0+vGacjGw2kox8H5ZsVGsfTNtU0c=";
    };
    modules = [{
      packages = {
        # Werror everything. This is a pain, see https://github.com/input-output-hk/haskell.nix/issues/519
        cardano-node-emulator.ghcOptions = [ "-Werror" ];
        cardano-node-socket-emulator.ghcOptions = [ "-Werror" ];
        freer-extras.ghcOptions = [ "-Werror" ];
        plutus-ledger.ghcOptions = [ "-Werror" ];
        plutus-script-utils.ghcOptions = [ "-Werror" ];
      };
    }];
  };

  project = lib.iogx.mkHaskellProject {
    inherit cabalProject;
    shellArgs = repoRoot.nix.shell;
    readTheDocs = {
      enable = true;
      siteFolder = "doc/read-the-docs-site";
    };
  };

in

project
