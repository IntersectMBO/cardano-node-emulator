{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Node.Socket.Emulator.Params where

import Cardano.Api.Genesis (ShelleyGenesis)
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Node.Emulator.Internal.Node.Params
import Cardano.Node.Socket.Emulator.Types
import Data.Aeson (FromJSON, eitherDecode)
import Data.ByteString.Lazy qualified as BSL

type ShelleyConfigUpdater = ShelleyGenesis StandardCrypto -> ShelleyGenesis StandardCrypto

fromNodeServerConfig :: ShelleyConfigUpdater -> NodeServerConfig -> IO Params
fromNodeServerConfig updateShelley NodeServerConfig{nscShelleyGenesisPath, nscAlonzoGenesisPath, nscConwayGenesisPath} = do
  shelleyConfig <- readConfig emulatorShelleyGenesisDefaults nscShelleyGenesisPath
  alonzoConfig <- readConfig emulatorAlonzoGenesisDefaults nscAlonzoGenesisPath
  conwayConfig <- readConfig emulatorConwayGenesisDefaults nscConwayGenesisPath
  pure $
    paramsFromConfig $
      mkLatestTransitionConfig (updateShelley shelleyConfig) alonzoConfig conwayConfig

readConfig :: (FromJSON a) => a -> Maybe FilePath -> IO a
readConfig a = maybe (pure a) readPP
  where
    readPP path = do
      bs <- BSL.readFile path
      case eitherDecode bs of
        Left err ->
          error $
            "Error reading JSON file: "
              ++ show path
              ++ " ("
              ++ err
              ++ ")"
        Right params -> pure params
