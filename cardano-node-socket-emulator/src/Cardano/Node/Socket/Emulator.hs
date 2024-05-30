{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}

module Cardano.Node.Socket.Emulator (
  main,
  prettyTrace,
  startTestnet,
) where

import Cardano.Api (NetworkId, NetworkMagic (NetworkMagic), toNetworkMagic)
import Cardano.Api.Genesis (ShelleyGenesis (sgNetworkMagic, sgSlotLength))
import Cardano.BM.Trace (Trace, stdoutTrace)
import Cardano.Node.Emulator.Internal.Node (SlotConfig (SlotConfig, scSlotLength, scSlotZeroTime))
import Cardano.Node.Emulator.Internal.Node.Params (keptBlocks, pSlotConfig)
import Cardano.Node.Socket.Emulator.Mock (slotCoordinator)
import Cardano.Node.Socket.Emulator.Params qualified as Params
import Cardano.Node.Socket.Emulator.Server qualified as Server
import Cardano.Node.Socket.Emulator.Types (
  AppState (..),
  CNSEServerLogMsg (..),
  NodeServerConfig (..),
  initialChainState,
 )
import Control.Concurrent (forkIO)
import Control.Monad (void)
import Control.Monad.Freer.Extras.Log (logInfo)
import Control.Monad.IO.Class (liftIO)
import Data.Default (def)
import Data.Map.Strict qualified as Map
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Units (Millisecond)
import Ledger.CardanoWallet (knownAddresses)
import Ledger.Value.CardanoAPI qualified as CardanoAPI
import Plutus.Monitoring.Util qualified as LM
import Prettyprinter (defaultLayoutOptions, layoutPretty, pretty)
import Prettyprinter.Render.Text (renderStrict)

main :: Trace IO CNSEServerLogMsg -> NodeServerConfig -> IO ()
main trace nodeServerConfig = core trace nodeServerConfig id

core :: Trace IO CNSEServerLogMsg -> NodeServerConfig -> Params.ShelleyConfigUpdater -> IO ()
core
  trace
  nodeServerConfig@NodeServerConfig
    { nscInitialTxWallets
    , nscSocketPath
    }
  updateShelley =
    LM.runLogEffects trace $ do
      -- make initial distribution of 1 billion Ada to all configured wallets
      let getAddress n = knownAddresses !! (fromIntegral n - 1)
          dist =
            Map.fromList $
              zip (getAddress <$> nscInitialTxWallets) (repeat (CardanoAPI.adaValueOf 1_000_000_000))
      params <- liftIO $ Params.fromNodeServerConfig updateShelley nodeServerConfig
      initialState <- initialChainState params dist
      let appState = AppState initialState mempty params
      serverHandler <-
        liftIO $
          Server.runServerNode
            (LM.convertLog ProcessingEmulatorMsg trace)
            nscSocketPath
            (keptBlocks params)
            appState

      let SlotConfig{scSlotZeroTime, scSlotLength} = pSlotConfig params
      logInfo $
        StartingSlotCoordination
          (posixSecondsToUTCTime $ realToFrac scSlotZeroTime / 1_000)
          (fromInteger scSlotLength :: Millisecond)
      void $ liftIO $ forkIO $ slotCoordinator (pSlotConfig params) serverHandler

      logInfo StartingCNSEServer

prettyTrace :: Trace IO CNSEServerLogMsg
prettyTrace = LM.convertLog (renderStrict . layoutPretty defaultLayoutOptions . pretty) stdoutTrace

startTestnet :: FilePath -> Integer -> NetworkId -> IO ()
startTestnet socketPath slotLength networkId =
  let
    updateShelley config =
      config
        { sgSlotLength = fromIntegral slotLength / 1000.0
        , sgNetworkMagic = case toNetworkMagic networkId of NetworkMagic nm -> nm
        }
   in
    core prettyTrace def{nscSocketPath = socketPath} updateShelley
