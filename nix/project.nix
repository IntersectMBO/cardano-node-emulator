{ repoRoot, inputs, pkgs, lib, system }:

let

  cabalProject = pkgs.haskell-nix.cabalProject' {
    name = "cardano-node-emulator";
    src = ../.;
    # If you change this version number, adapt the GHA CI accordingly (.github/workflows/haskell.yml)
    compiler-nix-name = lib.mkDefault "ghc965";
    shell.withHoogle = false;
    inputMap = {
      "https://chap.intersectmbo.org/" = inputs.CHaP;
    };
    sha256map = {
      "https://github.com/IntersectMBO/cardano-node"."a158a679690ed8b003ee06e1216ac8acd5ab823d" = "sha256-uY7wPyCgKuIZcGu0+vGacjGw2kox8H5ZsVGsfTNtU0c=";
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
