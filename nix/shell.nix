{ repoRoot, inputs, pkgs, lib, system }:

_cabalProject:

{
  name = "cardano-node-emulator";

  packages = [
    inputs.mithril.packages.mithril-client
  ];

  preCommit = {
    fourmolu.enable = true;
    shellcheck.enable = true;
    cabal-fmt.enable = true;
    optipng.enable = true;
    nixpkgs-fmt.enable = true;
  };
}
