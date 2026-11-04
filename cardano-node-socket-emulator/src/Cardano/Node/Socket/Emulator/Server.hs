{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Node.Socket.Emulator.Server
  ( ServerHandler,
    runServerNode,
    processBlock,
    modifySlot,
    addTx,
    processChainEffects,
  )
where

import Cardano.Api qualified as C
import Cardano.BM.Data.Trace (Trace)
import Cardano.Node.Emulator.API qualified as E
import Cardano.Node.Emulator.Internal.API qualified as E
import Cardano.Node.Emulator.Internal.Node.Chain qualified as Chain
import Cardano.Node.Emulator.Internal.Node.Validation qualified as Validation
import Cardano.Node.Emulator.Log qualified as E
import Cardano.Node.Socket.Emulator.Query (handleQuery)
import Cardano.Node.Socket.Emulator.Types
  ( AppState (..),
    BlockId (BlockId),
    SocketEmulatorState (..),
    Tip,
    blockId,
    chainSyncCodec,
    doNothingResponderProtocol,
    emulatorState,
    getChannel,
    getTip,
    nodeToClientVersion,
    nodeToClientVersionData,
    runChainEffects,
    setTip,
    socketEmulatorState,
    stateQueryCodec,
    toCardanoBlock,
    txSubmissionCodec,
  )
import Cardano.Slotting.Slot (SlotNo (..), WithOrigin (..))
import Control.Concurrent
  ( MVar,
    ThreadId,
    forkIO,
    modifyMVar_,
    newMVar,
    readMVar,
  )
import Control.Concurrent.Async (async, wait)
import Control.Concurrent.STM
  ( STM,
    TChan,
    TQueue,
    atomically,
    cloneTChan,
    newTQueueIO,
    readTChan,
    readTQueue,
    retry,
    tryReadTChan,
    writeTChan,
    writeTQueue,
  )
import Control.Exception (throwIO)
import Control.Lens (over, (.~), (^.))
import Control.Monad (forever, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Tracer (nullTracer)
import Data.ByteString.Lazy qualified as LBS
import Data.Coerce (coerce)
import Data.Foldable (traverse_)
import Data.List (intersect)
import Data.Maybe (listToMaybe)
import Data.SOP.Strict (NS (S, Z))
import Data.Void (Void)
import Ledger (Block, CardanoTx (..), Slot (..))
import Network.Mux.Types (Mode (ResponderMode))
import Ouroboros.Consensus.Cardano.Block (CardanoBlock)
import Ouroboros.Consensus.Cardano.Block qualified as Consensus
import Ouroboros.Consensus.HardFork.Combinator qualified as Consensus
import Ouroboros.Consensus.Ledger.Query (Query (..))
import Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr)
import Ouroboros.Consensus.Shelley.Eras (StandardCrypto)
import Ouroboros.Consensus.Shelley.Ledger qualified as Shelley
import Ouroboros.Consensus.TypeFamilyWrappers (WrapApplyTxErr (WrapApplyTxErr))
import Ouroboros.Network.Block (Point (..), pointSlot)
import Ouroboros.Network.Block qualified as O
import Ouroboros.Network.IOManager
import Ouroboros.Network.Mux
import Ouroboros.Network.NodeToClient
  ( NodeToClientProtocols (..),
    nodeToClientCodecCBORTerm,
    nullErrorPolicies,
    versionedNodeToClientProtocols,
  )
import Ouroboros.Network.Point qualified as OP (Block (..))
import Ouroboros.Network.Protocol.ChainSync.Server
  ( ChainSyncServer (..),
    ServerStIdle (..),
    ServerStIntersect (..),
    ServerStNext (..),
  )
import Ouroboros.Network.Protocol.ChainSync.Server qualified as ChainSync
import Ouroboros.Network.Protocol.Handshake.Codec
import Ouroboros.Network.Protocol.Handshake.Version
import Ouroboros.Network.Protocol.LocalStateQuery.Server qualified as Query
import Ouroboros.Network.Protocol.LocalStateQuery.Server qualified as StateQuery
import Ouroboros.Network.Protocol.LocalStateQuery.Type qualified as LSQ (State (StateIdle))
import Ouroboros.Network.Protocol.LocalTxSubmission.Server qualified as TxSubmission
import Ouroboros.Network.Protocol.LocalTxSubmission.Type qualified as TxSubmission
import Ouroboros.Network.Snocket
import Ouroboros.Network.Socket
import Plutus.Monitoring.Util qualified as LM
import Polysemy (Embed, Members, Sem, insertAt)
import Polysemy.Error (Error)
import Polysemy.Final (embedToFinal, runFinal)
import Polysemy.Reader (Reader, ask, runReader)
import Polysemy.State (State)
import Polysemy.Writer (Writer)

data CommandChannel = CommandChannel
  { ccCommand :: TQueue ServerCommand,
    ccResponse :: TQueue ServerResponse
  }

-- | Clone the original channel for each connected client, then use
--     this wrapper to make sure that no data is consumed from the
--     original channel.
newtype LocalChannel = LocalChannel (TChan Block)

-- | A handler used to pass around the path to the server
--     and channels used for controlling the server.
data ServerHandler = ServerHandler
  { shSocketPath :: FilePath,
    -- The client will send a `ServerCommand` and the server will
    -- respond with a `ServerResponse`.
    shCommandChannel :: CommandChannel
  }

-- | The commands that control the server. This API is not part of the client
--     interface, and in order to call them directly you will need access to the
--     returned ServerHandler
data ServerCommand
  = -- This command will add a new block by processing
    -- transactions in the memory pool.
    ProcessBlock
  | -- Set the slot number
    ModifySlot (Slot -> Slot)
  | -- Append a transaction to the transaction pool.
    AddTx (C.Tx C.ConwayEra)

instance Show ServerCommand where
  show = \case
    ProcessBlock -> "ProcessBlock"
    ModifySlot _ -> "ModifySlot"
    AddTx t -> "AddTx " <> show t

-- | The response from the server. Can be used for the information
--     passed back, or for synchronisation.
data ServerResponse
  = -- A block was added. We are using this for synchronization.
    BlockAdded Block
  | SlotChanged Slot
  deriving (Show)

processBlock :: (MonadIO m) => ServerHandler -> m Block
processBlock ServerHandler {shCommandChannel} = do
  liftIO $ atomically $ writeTQueue (ccCommand shCommandChannel) ProcessBlock
  -- Wait for the server to finish processing blocks.
  liftIO $
    atomically $
      readTQueue (ccResponse shCommandChannel) >>= \case
        BlockAdded block -> pure block
        _ -> retry

modifySlot :: (MonadIO m) => (Slot -> Slot) -> ServerHandler -> m Slot
modifySlot f ServerHandler {shCommandChannel} = do
  liftIO $ atomically $ writeTQueue (ccCommand shCommandChannel) $ ModifySlot f
  -- Wait for the server to finish changing the slot.
  liftIO $
    atomically $
      readTQueue (ccResponse shCommandChannel) >>= \case
        SlotChanged slot -> pure slot
        _ -> retry

addTx :: (MonadIO m) => ServerHandler -> C.Tx C.ConwayEra -> m ()
addTx ServerHandler {shCommandChannel} tx = do
  liftIO $ atomically $ writeTQueue (ccCommand shCommandChannel) $ AddTx tx

{- Create a thread that keeps the number of blocks in the channel to the maximum
   limit of K -}
pruneChain :: (MonadIO m) => Integer -> TChan Block -> m ThreadId
pruneChain k original = do
  localChannel <- liftIO $ atomically $ cloneTChan original
  liftIO . forkIO $ go k localChannel
  where
    go :: (MonadIO m) => Integer -> TChan Block -> m ()
    go k' localChannel = do
      -- Wait for data on the channel
      _ <- liftIO $ atomically $ readTChan localChannel
      if k' == 0
        then {- When the counter reaches zero, there are K blocks in the
                original channel and we start to remove the oldest stored
                block by reading it. -}
          do
            liftIO $ atomically (readTChan original) >> go 0 localChannel
        else do
          go (k' - 1) localChannel

processChainEffects ::
  Trace IO E.EmulatorMsg ->
  MVar AppState ->
  Sem
    '[ Error E.EmulatorError,
       Reader E.EmulatorParams,
       Writer E.EmulatorLogs,
       State E.EmulatorState,
       Embed IO
     ]
    a ->
  IO a
processChainEffects trace stateVar eff = do
  (events, result) <- runChainEffects stateVar eff
  LM.runLogEffects trace $ traverse_ E.logMsg events
  either throwIO pure result

handleCommand ::
  (MonadIO m) =>
  Trace IO E.EmulatorMsg ->
  CommandChannel ->
  MVar AppState ->
  m ()
handleCommand trace CommandChannel {ccCommand, ccResponse} mvAppState =
  liftIO $
    atomically (readTQueue ccCommand) >>= \case
      AddTx tx -> processChainEffects trace mvAppState $ E.queueTx $ CardanoEmulatorEraTx tx
      ModifySlot f -> do
        s <- processChainEffects trace mvAppState $ E.modifySlot f
        atomically $
          writeTQueue ccResponse (SlotChanged s)
      ProcessBlock -> do
        block <- processChainEffects trace mvAppState E.processBlock
        setTip mvAppState block
        ch <- getChannel mvAppState
        atomically $ do
          writeTChan ch block
          writeTQueue ccResponse (BlockAdded block)

-- | Start the server in a new thread, and return a server handler
--     used to control the server
runServerNode ::
  (MonadIO m) =>
  Trace IO E.EmulatorMsg ->
  FilePath ->
  Integer ->
  AppState ->
  m ServerHandler
runServerNode trace shSocketPath k initialState = liftIO $ do
  serverState <- newMVar initialState
  shCommandChannel <- CommandChannel <$> newTQueueIO <*> newTQueueIO
  globalChannel <- getChannel serverState
  void $ forkIO . void $ protocolLoop shSocketPath serverState
  void $ forkIO . forever $ handleCommand trace shCommandChannel serverState
  void $ pruneChain k globalChannel
  pure $ ServerHandler {shSocketPath, shCommandChannel}

-- -- * ChainSync protocol

-- {- A monad for running all code executed when a state
--    transition is invoked. It makes the implementation of
--    state transitions easier to read. -}

type ChainSyncMonad = Sem '[Reader (MVar AppState), Embed IO]

runChainSync :: MVar AppState -> ChainSyncMonad a -> IO a
runChainSync mvar = runFinal . embedToFinal . runReader mvar . insertAt @2

{- The initial state of the protocol. You can move into
   requesting the next block or reset state by searching for an
   intersection.
-}
idleState ::
  ( Members '[Reader (MVar AppState), Embed IO] effs,
    block ~ CardanoBlock StandardCrypto
  ) =>
  LocalChannel ->
  Sem effs (ServerStIdle block (Point block) Tip (Sem effs) ())
idleState channel' = do
  pure
    ServerStIdle
      { recvMsgRequestNext = nextState channel',
        recvMsgFindIntersect = findIntersect,
        recvMsgDoneClient = return ()
      }

{- Get the next block, either immediately (the Just/Left branch)
   or within a monad (IO, in our case) where you can wait for the
   next block (Nothing/Right branch)
-}
nextState ::
  ( Members '[Reader (MVar AppState), Embed IO] effs,
    block ~ CardanoBlock StandardCrypto
  ) =>
  LocalChannel ->
  Sem
    effs
    ( Either
        (ServerStNext block (Point block) Tip (Sem effs) ())
        (Sem effs (ServerStNext block (Point block) Tip (Sem effs) ()))
    )
nextState localChannel@(LocalChannel channel') = do
  chainState <- ask
  tip' <- getTip chainState
  (liftIO . atomically $ tryReadTChan channel') >>= \case
    Nothing -> do
      Right . pure <$> do
        nextBlock <- liftIO . atomically $ readTChan channel'
        cBlock <- liftIO $ toCardanoBlock tip' nextBlock
        sendRollForward localChannel tip' cBlock
    Just nextBlock -> do
      cBlock <- liftIO $ toCardanoBlock tip' nextBlock
      Left <$> sendRollForward localChannel tip' cBlock

{- This protocol state will search for a block intersection
   with some client provided blocks. When an intersection is found
   the client state is reset to the new offset (the Just branch)
   or to the genesis block if no intersection was found.
-}
findIntersect ::
  ( Members '[Reader (MVar AppState), Embed IO] effs,
    block ~ CardanoBlock StandardCrypto
  ) =>
  [Point block] ->
  Sem effs (ServerStIntersect block (Point block) Tip (Sem effs) ())
findIntersect clientPoints = do
  mvState <- ask
  appState <- liftIO $ readMVar mvState
  let chainState = appState ^. socketEmulatorState . emulatorState . E.esChainState
      blocks = Chain._chainNewestFirst chainState
      slot = Validation.getSlot (Chain._ledgerState chainState)
  serverPoints <- getChainPoints blocks slot
  let point =
        listToMaybe $
          intersect
            serverPoints
            clientPoints
  tip' <- getTip mvState
  pure $ case point of
    Nothing ->
      SendMsgIntersectNotFound
        O.TipGenesis
        -- No intersection found. Resume from origin.
        (ChainSyncServer $ cloneChainFrom 0 >>= idleState)
    Just point' ->
      SendMsgIntersectFound
        point'
        tip'
        -- Resuming from point'.
        (ChainSyncServer $ cloneChainFrom (pointOffset point') >>= idleState)

{- This is a wrapper around the creation of a `ServerStNext`
-}
sendRollForward ::
  ( Members '[Reader (MVar AppState), Embed IO] effs,
    block ~ CardanoBlock StandardCrypto
  ) =>
  LocalChannel ->
  Tip -> -- tip
  block -> -- current
  Sem effs (ServerStNext block (Point block) Tip (Sem effs) ())
sendRollForward channel' tip' current =
  pure $
    SendMsgRollForward
      current
      tip'
      (ChainSyncServer (idleState channel'))

{- This is the state for a new connection. For now we start with
   slot 0, and in idleState. This will probably change, since it
   makes more sense to start in the `StIntersect` state.
-}
chainSyncServer ::
  ( Members '[Reader (MVar AppState), Embed IO] effs,
    block ~ CardanoBlock StandardCrypto
  ) =>
  ChainSyncServer block (Point block) Tip (Sem effs) ()
chainSyncServer =
  ChainSyncServer (cloneChainFrom 0 >>= idleState)

{- Use a `TChan` to model a broadcast channel of which we
   clone (with potentially varying offsets) for clients.
-}
cloneChainFrom ::
  forall effs.
  (Members '[Reader (MVar AppState), Embed IO] effs) =>
  Integer ->
  Sem effs LocalChannel
cloneChainFrom offset = LocalChannel <$> go
  where
    go :: Sem effs (TChan Block)
    go = do
      globalChannel <- ask >>= getChannel
      liftIO $ atomically $ do
        localChannel <- cloneTChan globalChannel
        consume localChannel offset

    consume :: TChan a -> Integer -> STM (TChan a)
    consume channel' ix | ix == 0 = pure channel'
    consume channel' ix =
      -- We should have all requested blocks available on the
      -- channel, for consumption.
      tryReadTChan channel' >> consume channel' (ix - 1)

-- * Protocol setup

{- The node protocols always run in the IO monad. I wanted to use a
   different monad stack (mainly to be able to pass the internal state
   in a `MonadReader` and future proofing) so I wrote some hoisting
   functions for each of the states which transform the `ChainSyncMonad`
   into IO.
-}

hoistChainSync ::
  MVar AppState ->
  ChainSyncServer block (Point block) Tip ChainSyncMonad a ->
  ChainSyncServer block (Point block) Tip IO a
hoistChainSync internalState machine =
  ChainSyncServer
    { {- The basic idea is running the reader monad to remove it,
         leaving only IO, which is what we need. We do the same for all
         other states. -}
      runChainSyncServer =
        hoistStIdle internalState
          <$> runChainSync internalState (runChainSyncServer machine)
    }

hoistStIdle ::
  MVar AppState ->
  ServerStIdle block (Point block) Tip ChainSyncMonad a ->
  ServerStIdle block (Point block) Tip IO a
hoistStIdle internalState (ServerStIdle nextState' findIntersect' done) =
  ServerStIdle
    { recvMsgRequestNext =
        runChainSync internalState nextState' >>= \case
          Left stNext ->
            pure $ Left $ hoistStNext internalState stNext
          Right mNext -> do
            stNext <- runChainSync internalState mNext
            pure $ Right $ pure $ hoistStNext internalState stNext,
      recvMsgFindIntersect = \points ->
        hoistStIntersect internalState
          <$> runChainSync internalState (findIntersect' points),
      recvMsgDoneClient = runChainSync internalState done
    }

hoistStIntersect ::
  MVar AppState ->
  ServerStIntersect block (Point block) Tip ChainSyncMonad a ->
  ServerStIntersect block (Point block) Tip IO a
hoistStIntersect internalState = \case
  SendMsgIntersectFound point tip' nextState' ->
    SendMsgIntersectFound point tip' (hoistChainSync internalState nextState')
  SendMsgIntersectNotFound tip' nextState' ->
    SendMsgIntersectNotFound tip' (hoistChainSync internalState nextState')

hoistStNext ::
  MVar AppState ->
  ServerStNext block (Point block) Tip ChainSyncMonad a ->
  ServerStNext block (Point block) Tip IO a
hoistStNext internalState = \case
  SendMsgRollForward header tip' nextState' ->
    SendMsgRollForward header tip' (hoistChainSync internalState nextState')
  SendMsgRollBackward header tip' nextState' ->
    SendMsgRollBackward header tip' (hoistChainSync internalState nextState')

{- This is boilerplate code that sets up the node protocols,
   you can find in:
     ouroboros-network/ouroboros-network/demo/chain-sync.hs
-}

protocolLoop ::
  (MonadIO m) =>
  FilePath ->
  MVar AppState ->
  m Void
protocolLoop socketPath internalState = liftIO $ withIOManager $ \iocp -> do
  networkState <- newNetworkMutableState
  _ <- async $ cleanNetworkMutableState networkState
  withServerNode
    (localSnocket iocp)
    makeLocalBearer
    (\_ _ -> pure ())
    nullNetworkServerTracers
    networkState
    (AcceptedConnectionsLimit maxBound maxBound 0)
    (localAddressFromPath socketPath)
    nodeToClientHandshakeCodec
    noTimeLimitsHandshake
    (cborTermVersionDataCodec nodeToClientCodecCBORTerm)
    (HandshakeCallbacks acceptableVersion queryVersion)
    ( SomeResponderApplication
        <$> versionedNodeToClientProtocols
          nodeToClientVersion
          nodeToClientVersionData
          (nodeToClientProtocols internalState)
    )
    nullErrorPolicies
    $ \_ serverAsync -> wait serverAsync

nodeToClientProtocols ::
  MVar AppState ->
  NodeToClientProtocols 'ResponderMode LocalAddress LBS.ByteString IO Void ()
nodeToClientProtocols internalState =
  NodeToClientProtocols
    { localChainSyncProtocol = chainSync internalState,
      localTxSubmissionProtocol = txSubmission internalState,
      localStateQueryProtocol = stateQuery internalState,
      localTxMonitorProtocol = doNothingResponderProtocol
    }

chainSync ::
  MVar AppState ->
  RunMiniProtocolWithMinimalCtx 'ResponderMode LocalAddress LBS.ByteString IO Void ()
chainSync mvChainState =
  ResponderProtocolOnly $
    mkMiniProtocolCbFromPeer $
      const
        ( nullTracer,
          chainSyncCodec,
          ChainSync.chainSyncServerPeer (hoistChainSync mvChainState chainSyncServer)
        )

txSubmission ::
  MVar AppState ->
  RunMiniProtocolWithMinimalCtx 'ResponderMode LocalAddress LBS.ByteString IO Void ()
txSubmission mvChainState =
  ResponderProtocolOnly $
    mkMiniProtocolCbFromPeer $
      const
        ( nullTracer,
          txSubmissionCodec,
          TxSubmission.localTxSubmissionServerPeer
            (pure $ txSubmissionServer mvChainState)
        )

stateQuery ::
  MVar AppState ->
  RunMiniProtocolWithMinimalCtx 'ResponderMode LocalAddress LBS.ByteString IO Void ()
stateQuery mvChainState =
  ResponderProtocolOnly $
    mkMiniProtocolCbFromPeerSt $
      const
        ( nullTracer,
          stateQueryCodec,
          LSQ.StateIdle,
          Query.localStateQueryServerPeer
            (stateQueryServer mvChainState)
        )

-- * Computing intersections

-- Given a `Point` find its offset into the chain.
pointOffset ::
  Point block ->
  Integer
pointOffset pt =
  case pointSlot pt of
    Origin -> 0
    At (SlotNo s) -> fromIntegral s

-- Currently selects all points from the blockchain.
getChainPoints ::
  (MonadIO m, block ~ CardanoBlock StandardCrypto) =>
  [Block] ->
  Slot ->
  m [Point block]
getChainPoints chain slot = do
  pure $ zipWith mkPoint [slot, slot - 1 .. 1] chain ++ [Point Origin]
  where
    mkPoint s block =
      Point
        ( At
            ( OP.Block
                (fromIntegral s)
                (coerce $ blockId block)
            )
        )

-- * TxSubmission protocol

{- I did not use the same approach for this protocol as I did
   for the `ChainSync`. This protocol has only one state and
   it is much simpler.
-}

txSubmissionServer ::
  (block ~ CardanoBlock StandardCrypto) =>
  MVar AppState ->
  TxSubmission.LocalTxSubmissionServer (Shelley.GenTx block) (ApplyTxErr block) IO ()
txSubmissionServer state =
  TxSubmission.LocalTxSubmissionServer
    { TxSubmission.recvMsgSubmitTx = fmap (,txSubmissionServer state) . submitTx state,
      TxSubmission.recvMsgDone = ()
    }

-- Remove if Cardano.Api.Internal.InMode is exposed again
fromConsensusGenTx :: () => (Consensus.CardanoBlock StandardCrypto ~ block) => Consensus.GenTx block -> C.TxInMode
fromConsensusGenTx = \case
  Consensus.HardForkGenTx (Consensus.OneEraGenTx (Z tx')) ->
    C.TxInByronSpecial tx'
  Consensus.HardForkGenTx (Consensus.OneEraGenTx (S (Z tx'))) ->
    let Shelley.ShelleyTx _txid shelleyEraTx = tx'
     in C.TxInMode C.ShelleyBasedEraShelley (C.ShelleyTx C.ShelleyBasedEraShelley shelleyEraTx)
  Consensus.HardForkGenTx (Consensus.OneEraGenTx (S (S (Z tx')))) ->
    let Shelley.ShelleyTx _txid shelleyEraTx = tx'
     in C.TxInMode C.ShelleyBasedEraAllegra (C.ShelleyTx C.ShelleyBasedEraAllegra shelleyEraTx)
  Consensus.HardForkGenTx (Consensus.OneEraGenTx (S (S (S (Z tx'))))) ->
    let Shelley.ShelleyTx _txid shelleyEraTx = tx'
     in C.TxInMode C.ShelleyBasedEraMary (C.ShelleyTx C.ShelleyBasedEraMary shelleyEraTx)
  Consensus.HardForkGenTx (Consensus.OneEraGenTx (S (S (S (S (Z tx')))))) ->
    let Shelley.ShelleyTx _txid shelleyEraTx = tx'
     in C.TxInMode C.ShelleyBasedEraAlonzo (C.ShelleyTx C.ShelleyBasedEraAlonzo shelleyEraTx)
  Consensus.HardForkGenTx (Consensus.OneEraGenTx (S (S (S (S (S (Z tx'))))))) ->
    let Shelley.ShelleyTx _txid shelleyEraTx = tx'
     in C.TxInMode C.ShelleyBasedEraBabbage (C.ShelleyTx C.ShelleyBasedEraBabbage shelleyEraTx)
  Consensus.HardForkGenTx (Consensus.OneEraGenTx (S (S (S (S (S (S (Z tx')))))))) ->
    let Shelley.ShelleyTx _txid shelleyEraTx = tx'
     in C.TxInMode C.ShelleyBasedEraConway (C.ShelleyTx C.ShelleyBasedEraConway shelleyEraTx)

submitTx ::
  (block ~ CardanoBlock StandardCrypto) =>
  MVar AppState ->
  Shelley.GenTx block ->
  IO (TxSubmission.SubmitResult (ApplyTxErr block))
submitTx state tx = case fromConsensusGenTx tx of
  C.TxInMode C.ShelleyBasedEraConway shelleyTx -> do
    AppState
      (SocketEmulatorState (E.EmulatorState chainState _ _) _ _)
      _
      params <-
      readMVar state
    case Validation.validateAndApplyTx params (Chain._ledgerState chainState) shelleyTx of
      Left err ->
        pure $
          TxSubmission.SubmitFail
            ( Consensus.HardForkApplyTxErrFromEra
                (Consensus.OneEraApplyTxErr (S (S (S (S (S (S (Z (WrapApplyTxErr err)))))))))
            )
      Right (ls', _) -> do
        let ctx = CardanoEmulatorEraTx shelleyTx
        modifyMVar_
          state
          ( pure
              . over
                (socketEmulatorState . emulatorState . E.esChainState)
                (Chain.addTxToPool ctx . (Chain.ledgerState .~ ls'))
          )
        pure TxSubmission.SubmitSuccess
  _ -> pure TxSubmission.SubmitSuccess -- should be SubmitFail HardForkApplyTxErrWrongEra, but the Mismatch type is complicated

stateQueryServer ::
  (block ~ CardanoBlock StandardCrypto) =>
  MVar AppState ->
  StateQuery.LocalStateQueryServer block (Point block) (Query block) IO ()
stateQueryServer state = Query.LocalStateQueryServer {Query.runLocalStateQueryServer = pure idle}
  where
    idle =
      Query.ServerStIdle
        { Query.recvMsgAcquire = acquiring,
          Query.recvMsgDone = pure ()
        }
    acquiring _point = pure $ Query.SendMsgAcquired ack
    ack =
      Query.ServerStAcquired
        { Query.recvMsgQuery = \q -> do
            res <- handleQuery state q
            pure $ Query.SendMsgResult res ack,
          Query.recvMsgReAcquire = acquiring,
          Query.recvMsgRelease = pure idle
        }
