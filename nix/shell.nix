{ repoRoot, inputs, pkgs, lib, system }:

_cabalProject: 

let
  cardano-cli = inputs.cardano-node.legacyPackages.cardano-cli;
  cardano-node = inputs.cardano-node.legacyPackages.cardano-node;
in
{
  name = "cardano-node-emulator";

  packages = [
    cardano-cli
    cardano-node
    inputs.mithril.packages.mithril-client
  ];

  env = {
    CARDANO_CLI = "${cardano-cli}/bin/cardano-cli";
    CARDANO_NODE = "${cardano-node}/bin/cardano-node";
  };

  preCommit = {
    fourmolu.enable = true;
    shellcheck.enable = true;
    cabal-fmt.enable = true;
    png-optimization.enable = true;
    nixpkgs-fmt.enable = true;
  };
}
