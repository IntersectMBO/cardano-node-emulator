{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Node.Socket.Emulator.Params where

import Cardano.Node.Emulator.Internal.Node.Params
import Cardano.Node.Socket.Emulator.Types
import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy qualified as BSL

fromNodeServerConfig :: NodeServerConfig -> IO Params
fromNodeServerConfig NodeServerConfig{nscSlotConfig, nscNetworkId, nscProtocolParametersJsonPath, nscEpochSize} = do
  protocolParameters <- readProtocolParameters nscProtocolParametersJsonPath
  pure $ Params nscSlotConfig protocolParameters nscNetworkId nscEpochSize

readProtocolParameters :: Maybe FilePath -> IO PParams
readProtocolParameters = maybe (pure defaultPParams) readPP
  where
    readPP path = do
      bs <- BSL.readFile path
      case eitherDecode bs of
        Left err ->
          error $
            "Error reading protocol parameters JSON file: "
              ++ show path
              ++ " ("
              ++ err
              ++ ")"
        Right params -> pure params
