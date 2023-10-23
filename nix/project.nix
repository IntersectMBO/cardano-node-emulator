{ repoRoot, inputs, pkgs, lib, system }:

let 

  cabalProject = pkgs.haskell-nix.cabalProject' {
    name = "cardano-node-emulator";
    src = ../.;
    compiler-nix-name = lib.mkDefault "ghc928";
    shell.withHoogle = false;
    inputMap = {
      "https://input-output-hk.github.io/cardano-haskell-packages" = inputs.iogx.inputs.CHaP;
    };
    sha256map = {
      "https://github.com/input-output-hk/cardano-node"."a158a679690ed8b003ee06e1216ac8acd5ab823d" = "sha256-uY7wPyCgKuIZcGu0+vGacjGw2kox8H5ZsVGsfTNtU0c=";
    };
    modules = [{
      packages = {
        # cardano-node-emulator.doHaddock = meta.enableHaddock;
        # cardano-node-socket-emulator.doHaddock = meta.enableHaddock;
        # freer-extras.doHaddock = meta.enableHaddock;
        # plutus-ledger.doHaddock = meta.enableHaddock;
        # plutus-script-utils.doHaddock = meta.enableHaddock;
        # cardano-node-emulator.flags.defer-plugin-errors = meta.enableHaddock;
        # cardano-node-socket-emulator.flags.defer-plugin-errors = meta.enableHaddock;
        # freer-extras.flags.defer-plugin-errors = meta.enableHaddock;
        # plutus-ledger.flags.defer-plugin-errors = meta.enableHaddock;
        # plutus-script-utils.flags.defer-plugin-errors = meta.enableHaddock;
        
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
      siteFolder = "docs/read-the-docs";
    };
  };

in 

  project 