{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Node.Socket.Emulator (
  main,
  prettyTrace,
  startTestnet,
) where

import Cardano.Api (NetworkId)
import Cardano.Api.Shelley qualified as C
import Cardano.BM.Trace (Trace, stdoutTrace)
import Cardano.Node.Emulator.Internal.Node (
  SlotConfig (SlotConfig, scSlotLength, scSlotZeroTime),
  pProtocolParams,
 )
import Cardano.Node.Socket.Emulator.API (API)
import Cardano.Node.Socket.Emulator.Mock (consumeEventHistory, healthcheck, slotCoordinator)
import Cardano.Node.Socket.Emulator.Params qualified as Params
import Cardano.Node.Socket.Emulator.Server qualified as Server
import Cardano.Node.Socket.Emulator.Types (
  AppState (..),
  CNSEServerLogMsg (..),
  NodeServerConfig (..),
  initialChainState,
 )
import Control.Concurrent (MVar, forkIO, newEmptyMVar, newMVar, putMVar, takeMVar)
import Control.Monad (void)
import Control.Monad.Freer.Extras.Delay (delayThread, handleDelayEffect)
import Control.Monad.Freer.Extras.Log (logInfo)
import Control.Monad.IO.Class (liftIO)
import Data.Default (def)
import Data.Function ((&))
import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy (Proxy))
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Units (Millisecond, Second)
import Ledger.CardanoWallet (knownAddresses)
import Ledger.Value.CardanoAPI qualified as CardanoAPI
import Network.Wai.Handler.Warp qualified as Warp
import Plutus.Monitoring.Util qualified as LM
import Prettyprinter (defaultLayoutOptions, layoutPretty, pretty)
import Prettyprinter.Render.Text (renderStrict)
import Servant (Application, hoistServer, serve, (:<|>) ((:<|>)))
import Servant.Client (BaseUrl (baseUrlPort))

app
  :: Trace IO CNSEServerLogMsg
  -> MVar AppState
  -> Application
app trace stateVar =
  serve (Proxy @API) $
    hoistServer
      (Proxy @API)
      (liftIO . Server.processChainEffects (LM.convertLog ProcessingEmulatorMsg trace) stateVar)
      (healthcheck :<|> consumeEventHistory stateVar)

main :: Trace IO CNSEServerLogMsg -> NodeServerConfig -> IO () -> IO ()
main
  trace
  nodeServerConfig@NodeServerConfig
    { nscBaseUrl
    , nscSlotConfig
    , nscKeptBlocks
    , nscInitialTxWallets
    , nscSocketPath
    }
  whenStarted = LM.runLogEffects trace $ do
    -- make initial distribution of 1 billion Ada to all configured wallets
    let getAddress n = knownAddresses !! (fromIntegral n - 1)
        dist =
          Map.fromList $
            zip (getAddress <$> nscInitialTxWallets) (repeat (CardanoAPI.adaValueOf 1_000_000_000))
    initialState <- initialChainState dist
    params <- liftIO $ Params.fromNodeServerConfig nodeServerConfig
    let appState = AppState initialState mempty params
    serverHandler <-
      liftIO $
        Server.runServerNode
          (LM.convertLog ProcessingEmulatorMsg trace)
          nscSocketPath
          nscKeptBlocks
          appState
    serverState <- liftIO $ newMVar appState
    handleDelayEffect $ delayThread (2 :: Second)

    let SlotConfig{scSlotZeroTime, scSlotLength} = nscSlotConfig
    logInfo $
      StartingSlotCoordination
        (posixSecondsToUTCTime $ realToFrac scSlotZeroTime / 1_000)
        (fromInteger scSlotLength :: Millisecond)
    void $ liftIO $ forkIO $ slotCoordinator nscSlotConfig serverHandler

    logInfo $ StartingCNSEServer $ baseUrlPort nscBaseUrl
    liftIO $ Warp.runSettings warpSettings $ app trace serverState
    where
      warpSettings = Warp.defaultSettings & Warp.setPort (baseUrlPort nscBaseUrl) & Warp.setBeforeMainLoop whenStarted

prettyTrace :: Trace IO CNSEServerLogMsg
prettyTrace = LM.convertLog (renderStrict . layoutPretty defaultLayoutOptions . pretty) stdoutTrace

startTestnet :: FilePath -> Integer -> NetworkId -> IO C.ProtocolParameters
startTestnet socketPath slotLength networkId =
  let
    config =
      def
        { nscSlotConfig =
            def
              { scSlotLength = slotLength
              }
        , nscSocketPath = socketPath
        , nscNetworkId = networkId
        }
   in
    do
      blocker <- newEmptyMVar
      void $ forkIO $ main prettyTrace config (putMVar blocker ())
      takeMVar blocker
      pProtocolParams <$> Params.fromNodeServerConfig config
