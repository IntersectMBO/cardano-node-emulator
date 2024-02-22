{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module exports data types for logging, events and configuration
module Cardano.Node.Socket.Emulator.Types where

import Cardano.Api (NetworkId, Value)
import Cardano.Chain.Slotting (EpochSlots (..))
import Cardano.Ledger.Block qualified as CL
import Cardano.Ledger.Era qualified as CL
import Cardano.Ledger.Shelley.API (Nonce (NeutralNonce), extractTx, unsafeMakeValidated)
import Cardano.Ledger.Slot (EpochSize)
import Cardano.Node.Emulator.API (
  EmulatorError,
  EmulatorLogs,
  EmulatorMsg,
  EmulatorState,
  EmulatorT,
  emptyEmulatorStateWithInitialDist,
  esChainState,
 )
import Cardano.Node.Emulator.Internal.Node.Chain qualified as EC
import Cardano.Node.Emulator.Internal.Node.Params (Params, emulatorEpochSize, testnet)
import Cardano.Node.Emulator.Internal.Node.TimeSlot (SlotConfig)
import Codec.Serialise (DeserialiseFailure)
import Codec.Serialise qualified as CBOR
import Control.Concurrent (MVar, modifyMVar_, putMVar, readMVar, takeMVar)
import Control.Concurrent.STM
import Control.Lens (makeLenses, view, (&), (.~), (^.))
import Control.Monad (forever)
import Control.Monad.Class.MonadST (MonadST)
import Control.Monad.Class.MonadTimer (MonadDelay (threadDelay), MonadTimer)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.RWS.Strict (runRWST)
import Crypto.Hash (SHA256, hash)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.Extras qualified as JSON
import Data.ByteArray qualified as BA
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Short qualified as BS
import Data.Coerce (coerce)
import Data.Default (Default, def)
import Data.Foldable (toList)
import Data.Functor ((<&>))
import Data.Map qualified as Map
import Data.Maybe (listToMaybe)
import Data.Text qualified as Text
import Data.Time.Clock (UTCTime)
import Data.Time.Format.ISO8601 qualified as F
import Data.Time.Units (Millisecond)
import Data.Time.Units.Extra ()
import Data.Void (Void)
import GHC.Generics (Generic)
import Ledger (Block, CardanoTx, OnChainTx (..))
import Ledger.Address (CardanoAddress)
import Ledger.CardanoWallet
import Ledger.Test (testNetworkMagic)
import Network.TypedProtocol.Codec (Codec)
import Ouroboros.Consensus.Byron.Ledger qualified as Byron
import Ouroboros.Consensus.Cardano.Block (CardanoBlock, CodecConfig (..))
import Ouroboros.Consensus.Cardano.Block qualified as OC
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras (OneEraHash (..))
import Ouroboros.Consensus.Ledger.Query (Query)
import Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr)
import Ouroboros.Consensus.Network.NodeToClient (
  ClientCodecs,
  cChainSyncCodec,
  cStateQueryCodec,
  cTxSubmissionCodec,
  clientCodecs,
 )
import Ouroboros.Consensus.Node.NetworkProtocolVersion (
  BlockNodeToClientVersion,
  supportedNodeToClientVersions,
 )
import Ouroboros.Consensus.Protocol.Praos.Header qualified as Praos
import Ouroboros.Consensus.Shelley.Eras (StandardCrypto)
import Ouroboros.Consensus.Shelley.Ledger qualified as Shelley
import Ouroboros.Network.Block (Point)
import Ouroboros.Network.Block qualified as Ouroboros
import Ouroboros.Network.Mux
import Ouroboros.Network.NodeToClient (
  LocalAddress,
  NodeToClientVersion (..),
  NodeToClientVersionData (..),
 )
import Ouroboros.Network.Protocol.ChainSync.Type qualified as ChainSync
import Ouroboros.Network.Protocol.LocalStateQuery.Type qualified as StateQuery
import Ouroboros.Network.Protocol.LocalTxSubmission.Type qualified as TxSubmission
import Ouroboros.Network.Util.ShowProxy
import Prettyprinter (Pretty, pretty, viaShow, vsep, (<+>))
import Prettyprinter.Extras (PrettyShow (PrettyShow))

import Cardano.Protocol.TPraos.BHeader
import Cardano.Protocol.TPraos.OCert (KESPeriod (..))
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Shelley.Constants (defaultConstants)
import Test.Cardano.Ledger.Shelley.Generator.Presets (coreNodeKeys)
import Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators ()
import Test.Cardano.Protocol.TPraos.Create (mkBlock, mkOCert)

type Tip = Ouroboros.Tip (CardanoBlock StandardCrypto)

type TxPool = [CardanoTx]

data SocketEmulatorState = SocketEmulatorState
  { _emulatorState :: EmulatorState
  , _channel :: TChan Block
  , _tip :: Tip
  }
  deriving (Generic)

makeLenses ''SocketEmulatorState

instance Show SocketEmulatorState where
  -- Skip showing the full chain
  show SocketEmulatorState{_emulatorState, _tip} =
    "SocketEmulatorState { "
      <> show _emulatorState
      <> ", "
      <> show _tip
      <> " }"

-- | Node server configuration
data NodeServerConfig = NodeServerConfig
  { nscInitialTxWallets :: [WalletNumber]
  -- ^ The wallets that receive money from the initial transaction.
  , nscSocketPath :: FilePath
  -- ^ Path to the socket used to communicate with the server.
  , nscKeptBlocks :: Integer
  -- ^ The number of blocks to keep for replaying to newly connected clients
  , nscSlotConfig :: SlotConfig
  -- ^ Beginning of slot 0.
  , nscNetworkId :: NetworkId
  -- ^ NetworkId that's used with the CardanoAPI.
  , nscProtocolParametersJsonPath :: Maybe FilePath
  -- ^ Path to a JSON file containing the protocol parameters
  , nscEpochSize :: EpochSize
  -- ^ Number of slots per epoch
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

defaultNodeServerConfig :: NodeServerConfig
defaultNodeServerConfig =
  NodeServerConfig
    { nscInitialTxWallets =
        [ WalletNumber 1
        , WalletNumber 2
        , WalletNumber 3
        , WalletNumber 4
        , WalletNumber 5
        , WalletNumber 6
        , WalletNumber 7
        , WalletNumber 8
        , WalletNumber 9
        , WalletNumber 10
        ]
    , nscSocketPath = "/tmp/node-server.sock"
    , nscKeptBlocks = 100
    , nscSlotConfig = def
    , nscNetworkId = testnet
    , nscProtocolParametersJsonPath = Nothing
    , nscEpochSize = emulatorEpochSize
    }

instance Default NodeServerConfig where
  def = defaultNodeServerConfig

instance Pretty NodeServerConfig where
  pretty NodeServerConfig{nscSocketPath, nscNetworkId, nscKeptBlocks} =
    vsep
      [ "Socket:" <+> pretty nscSocketPath
      , "Network Id:" <+> viaShow nscNetworkId
      , "Security Param:" <+> pretty nscKeptBlocks
      ]

-- | Application State
data AppState = AppState
  { _socketEmulatorState :: SocketEmulatorState
  -- ^ blockchain state
  , _emulatorLogs :: EmulatorLogs
  -- ^ history of all log messages
  , _emulatorParams :: Params
  }
  deriving (Show)

makeLenses 'AppState

fromEmulatorChainState :: (MonadIO m) => EmulatorState -> m SocketEmulatorState
fromEmulatorChainState state = do
  ch <- liftIO $ atomically newTChan
  let chainNewestFirst = view (esChainState . EC.chainNewestFirst) state
  let currentSlot = view (esChainState . EC.chainCurrentSlot) state
  void $
    liftIO $
      mapM_ (atomically . writeTChan ch) chainNewestFirst
  pure $
    SocketEmulatorState
      { _channel = ch
      , _emulatorState = state
      , _tip = case listToMaybe chainNewestFirst of
          Nothing -> Ouroboros.TipGenesis
          Just block -> Ouroboros.Tip (fromIntegral currentSlot) (coerce $ blockId block) (fromIntegral currentSlot)
      }

-- | 'ChainState' with initial values
initialChainState :: (MonadIO m) => Map.Map CardanoAddress Value -> m SocketEmulatorState
initialChainState = fromEmulatorChainState . emptyEmulatorStateWithInitialDist

getChannel :: (MonadIO m) => MVar AppState -> m (TChan Block)
getChannel mv = liftIO (readMVar mv) <&> view (socketEmulatorState . channel)

-- Get the current tip.
getTip :: (MonadIO m) => MVar AppState -> m Tip
getTip mv = liftIO (readMVar mv) <&> view (socketEmulatorState . tip)

-- Set the new tip
setTip :: (MonadIO m) => MVar AppState -> Block -> m ()
setTip mv block = liftIO $ modifyMVar_ mv $ \oldState -> do
  let slot = oldState ^. socketEmulatorState . emulatorState . esChainState . EC.chainCurrentSlot
  pure $
    oldState
      & socketEmulatorState . tip
        .~ Ouroboros.Tip (fromIntegral slot) (coerce $ blockId block) (fromIntegral slot)

-- | Run all chain effects in the IO Monad
runChainEffects
  :: MVar AppState
  -> EmulatorT IO a
  -> IO (EmulatorLogs, Either EmulatorError a)
runChainEffects stateVar eff = do
  AppState (SocketEmulatorState oldState chan tip') events params <- liftIO $ takeMVar stateVar
  (a, newState, newEvents) <- runRWST (runExceptT eff) params oldState
  putMVar stateVar $
    AppState (SocketEmulatorState newState chan tip') (events <> newEvents) params
  pure (newEvents, a)

-- Logging ------------------------------------------------------------------------------------------------------------

{- | Top-level logging data type for structural logging
inside the CNSE server.
-}
data CNSEServerLogMsg
  = StartingSlotCoordination UTCTime Millisecond
  | StartingCNSEServer
  | ProcessingEmulatorMsg EmulatorMsg
  deriving (Generic, Show)

instance Pretty CNSEServerLogMsg where
  pretty = \case
    StartingSlotCoordination initialSlotTime slotLength ->
      "Starting slot coordination thread."
        <+> "Initial slot time:"
        <+> pretty (F.iso8601Show initialSlotTime)
        <+> "Slot length:"
        <+> viaShow slotLength
    StartingCNSEServer -> "Starting Cardano Node Emulator"
    ProcessingEmulatorMsg e -> "Processing emulator event:" <+> pretty e

-- | The node protocols require a block header type.
newtype BlockId = BlockId {getBlockId :: BS.ShortByteString}
  deriving (Eq, Ord, Generic)
  deriving newtype (CBOR.Serialise)
  deriving (Pretty) via (PrettyShow BlockId)

instance Show BlockId where
  show = Text.unpack . JSON.encodeByteString . BS.fromShort . getBlockId

-- | A hash of the block's contents.
blockId :: Block -> BlockId
blockId =
  BlockId
    . BS.toShort
    . BA.convert
    . hash @_ @SHA256
    . BSL.toStrict
    . CBOR.serialise

-- | Protocol versions
nodeToClientVersion :: NodeToClientVersion
nodeToClientVersion = NodeToClientV_16

{- | A temporary definition of the protocol version. This will be moved as an
argument to the client connection function in a future PR (the network magic
number matches the one in the test net created by scripts)
-}
nodeToClientVersionData :: NodeToClientVersionData
nodeToClientVersionData = NodeToClientVersionData{networkMagic = testNetworkMagic, query = False}

doNothingResponderProtocol
  :: (MonadTimer m)
  => RunMiniProtocolWithMinimalCtx
      'ResponderMode
      LocalAddress
      BSL.ByteString
      m
      Void
      a
doNothingResponderProtocol =
  ResponderProtocolOnly $
    MiniProtocolCb $
      \_ _ -> forever $ threadDelay 1_000_000

-- | Boilerplate codecs used for protocol serialisation.

{- | The number of epochSlots is specific to each blockchain instance. This value
is what the cardano main and testnet uses. Only applies to the Byron era.
-}
epochSlots :: EpochSlots
epochSlots = EpochSlots 21600

codecVersion :: BlockNodeToClientVersion (CardanoBlock StandardCrypto)
codecVersion = versionMap Map.! nodeToClientVersion
  where
    versionMap =
      supportedNodeToClientVersions
        (Proxy @(CardanoBlock StandardCrypto))

codecConfig :: CodecConfig (CardanoBlock StandardCrypto)
codecConfig =
  CardanoCodecConfig
    (Byron.ByronCodecConfig epochSlots)
    Shelley.ShelleyCodecConfig
    Shelley.ShelleyCodecConfig
    Shelley.ShelleyCodecConfig
    Shelley.ShelleyCodecConfig
    Shelley.ShelleyCodecConfig
    Shelley.ShelleyCodecConfig

nodeToClientCodecs
  :: forall m
   . (MonadST m)
  => ClientCodecs (CardanoBlock StandardCrypto) m
nodeToClientCodecs =
  clientCodecs codecConfig codecVersion nodeToClientVersion

{- | These codecs are currently used in the mock nodes and will
  probably soon get removed as the mock nodes are phased out.
-}
chainSyncCodec
  :: (block ~ CardanoBlock StandardCrypto)
  => Codec
      (ChainSync.ChainSync block (Point block) Tip)
      DeserialiseFailure
      IO
      BSL.ByteString
chainSyncCodec = cChainSyncCodec nodeToClientCodecs

txSubmissionCodec
  :: (block ~ CardanoBlock StandardCrypto)
  => Codec
      (TxSubmission.LocalTxSubmission (Shelley.GenTx block) (ApplyTxErr block))
      DeserialiseFailure
      IO
      BSL.ByteString
txSubmissionCodec = cTxSubmissionCodec nodeToClientCodecs

stateQueryCodec
  :: (block ~ CardanoBlock StandardCrypto)
  => Codec
      (StateQuery.LocalStateQuery block (Point block) (Query block))
      DeserialiseFailure
      IO
      BSL.ByteString
stateQueryCodec = cStateQueryCodec nodeToClientCodecs

toCardanoBlock
  :: Ouroboros.Tip (CardanoBlock StandardCrypto) -> Block -> IO (CardanoBlock StandardCrypto)
toCardanoBlock Ouroboros.TipGenesis _ = error "toCardanoBlock: TipGenesis not supported"
toCardanoBlock (Ouroboros.Tip curSlotNo _ curBlockNo) block = do
  prevHash <- generate (arbitrary :: Gen (HashHeader (OC.EraCrypto (OC.BabbageEra StandardCrypto))))
  let allPoolKeys = snd $ head $ coreNodeKeys defaultConstants
      kesPeriod = 1
      keyRegKesPeriod = 1
      ocert = mkOCert allPoolKeys 1 (KESPeriod kesPeriod)
      txs = extractTx . getOnChainTx <$> block
      CL.Block hdr1 bdy =
        mkBlock
          prevHash
          allPoolKeys
          txs
          curSlotNo
          curBlockNo
          NeutralNonce
          kesPeriod
          keyRegKesPeriod
          ocert
  let translateHeader (BHeader bhBody bhSig) = Praos.Header hBody hSig
        where
          hBody =
            Praos.HeaderBody
              { Praos.hbBlockNo = bheaderBlockNo bhBody
              , Praos.hbSlotNo = bheaderSlotNo bhBody
              , Praos.hbPrev = bheaderPrev bhBody
              , Praos.hbVk = bheaderVk bhBody
              , Praos.hbVrfVk = bheaderVrfVk bhBody
              , Praos.hbVrfRes = coerce $ bheaderEta bhBody
              , Praos.hbBodySize = bsize bhBody
              , Praos.hbBodyHash = bhash bhBody
              , Praos.hbOCert = bheaderOCert bhBody
              , Praos.hbProtVer = bprotver bhBody
              }
          hSig = coerce bhSig
  pure $ OC.BlockBabbage $ Shelley.mkShelleyBlock $ CL.Block (translateHeader hdr1) bdy

fromCardanoBlock :: CardanoBlock StandardCrypto -> Block
fromCardanoBlock (OC.BlockBabbage (Shelley.ShelleyBlock (CL.Block _ txSeq) _)) = map (OnChainTx . unsafeMakeValidated) . toList $ CL.fromTxSeq txSeq
fromCardanoBlock _ = []
