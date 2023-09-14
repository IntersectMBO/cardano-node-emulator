# This file is part of the IOGX template and is documented at the link below:
# https://www.github.com/input-output-hk/iogx#32-nixhaskellnix

{
  supportedCompilers = [ "ghc928" ];

  enableCrossCompilation = true;

  defaultChangelogPackages = [
    "cardano-node-emulator"
    "cardano-node-socket-emulator"
  ];
}
