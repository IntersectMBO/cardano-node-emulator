{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

-- | If you want to run the node emulator without using the `Contract` monad, this module provides a simple MTL-based interface.
module Cardano.Node.Emulator.API
  ( -- * Updating the blockchain
    queueTx,
    nextSlot,
    currentSlot,
    currentTimeRange,
    awaitSlot,
    awaitTime,

    -- * Querying the blockchain
    utxosAt,
    utxosAtPlutus,
    utxoAtTxIn,
    utxosAtTxIns,
    utxoAtTxOutRef,
    fundsAt,
    lookupDatum,
    getMemPoolEnv,
    getLedgerState,

    -- * Transactions
    signTx,

    -- * Logging
    logDebug,
    logInfo,
    logWarn,
    logError,

    -- * Types
    EmulatorEra,
    EmulatorState (EmulatorState),
    esChainState,
    esAddressMap,
    esDatumMap,
    EmulatedLedgerState,
    elsLedgerState,
    elsLedgerEnv,
    EmulatorError (..),
    EmulatorLogs,
    EmulatorMsg (..),
    emptyEmulatorState,
    emptyEmulatorStateWithInitialDist,
    EmulatorParams (..),
    getParams,
  )
where

import Cardano.Api qualified as C
import Cardano.Ledger.Shelley.API
  ( LedgerState,
    MempoolEnv,
  )
import Cardano.Node.Emulator.Internal.API
  ( ConcreteEmulatorEffs,
    EmulatorError (CustomError, ToCardanoError, ValidationError),
    EmulatorLogs,
    EmulatorState (EmulatorState),
    esAddressMap,
    esChainState,
    esDatumMap,
    handleChainEffects,
    modifySlot,
    processBlock,
  )
import Cardano.Node.Emulator.Internal.Node
  ( EmulatorParams (pConfig, pSlotConfig),
    posixTimeToEnclosingSlot,
    slotToBeginPOSIXTime,
    slotToEndPOSIXTime,
  )
import Cardano.Node.Emulator.Internal.Node.Chain qualified as E
  ( chainNewestFirst,
    emptyChainState,
    getCurrentSlot,
    index,
    ledgerState,
    queueTx,
  )
import Cardano.Node.Emulator.Internal.Node.Validation
  ( EmulatedLedgerState (elsLedgerEnv, elsLedgerState),
    elsLedgerEnvL,
    elsLedgerStateL,
    elsUtxoL,
    unsafeMakeValid,
  )
import Cardano.Node.Emulator.Log (LogLevel (Debug, Error, Info, Warning), LogMessage (LogMessage))
import Cardano.Node.Emulator.LogMessages
  ( EmulatorMsg (ChainEvent, GenericMsg, TxBalanceMsg),
    TxBalanceMsg (SigningTx, SubmittingTx),
  )
import Control.Lens (view, (%~), (&), (.~), (<>~), (^.))
import Control.Monad (void)
import Data.Aeson (ToJSON (toJSON))
import Data.Map (Map)
import Data.Map qualified as Map
import Ledger
  ( CardanoAddress,
    Datum,
    DatumHash,
    DecoratedTxOut,
    POSIXTime,
    Slot,
    TxOutRef,
    UtxoIndex,
  )
import Ledger.AddressMap qualified as AM
import Ledger.Index qualified as Index
import Ledger.Tx
  ( CardanoTx,
    TxOut,
    addCardanoTxWitness,
    cardanoTxOutValue,
    getCardanoTxData,
    toCtxUTxOTxOut,
    toDecoratedTxOut,
  )
import Ledger.Tx.CardanoAPI
  ( EmulatorEra,
    fromCardanoTxIn,
    fromPlutusIndex,
    toCardanoTxIn,
  )
import Polysemy (Members, Sem)
import Polysemy.Reader (ask, asks)
import Polysemy.State (get, gets)
import Polysemy.Writer (tell)

emptyEmulatorState :: EmulatorParams -> EmulatorState
emptyEmulatorState params = EmulatorState (E.emptyChainState params) mempty mempty

emptyEmulatorStateWithInitialDist :: EmulatorParams -> Map CardanoAddress C.Value -> EmulatorState
emptyEmulatorStateWithInitialDist params initialDist =
  let tx = Index.createGenesisTransaction initialDist
      vtx = unsafeMakeValid tx
      index = Index.insertBlock [vtx] mempty
   in emptyEmulatorState params
        & esChainState . E.chainNewestFirst %~ ([vtx] :)
        & esChainState . E.index .~ index
        & esChainState . E.ledgerState . elsUtxoL .~ fromPlutusIndex index
        & esAddressMap %~ AM.updateAllAddresses vtx
        & esDatumMap <>~ getCardanoTxData tx

getParams :: (Members ConcreteEmulatorEffs effs) => Sem effs EmulatorParams
getParams = ask

-- | Queue the transaction, it will be processed when @nextSlot@ is called.
queueTx :: (Members ConcreteEmulatorEffs effs) => CardanoTx -> Sem effs ()
queueTx tx = do
  logMsg Info $ TxBalanceMsg $ SubmittingTx tx
  handleChainEffects $ E.queueTx tx

-- | Process the queued transactions and increase the slot number.
nextSlot :: (Members ConcreteEmulatorEffs effs) => Sem effs ()
nextSlot = void $ processBlock >> modifySlot succ

-- | Get the current slot number of the emulated node.
currentSlot :: (Members ConcreteEmulatorEffs effs) => Sem effs Slot
currentSlot = handleChainEffects E.getCurrentSlot

-- | Get the time range of the current slot of the emulated node.
currentTimeRange :: (Members ConcreteEmulatorEffs effs) => Sem effs (POSIXTime, POSIXTime)
currentTimeRange = do
  slotConfig <- asks pSlotConfig
  slot <- currentSlot
  pure
    ( slotToBeginPOSIXTime slotConfig slot,
      slotToEndPOSIXTime slotConfig slot
    )

-- | Call `nextSlot` until the current slot number equals or exceeds the given slot number.
awaitSlot :: (Members ConcreteEmulatorEffs effs) => Slot -> Sem effs ()
awaitSlot s = do
  c <- currentSlot
  if s <= c
    then pure ()
    else do
      nextSlot
      awaitSlot s

-- | Call `nextSlot` until the given time has been reached.
awaitTime :: (Members ConcreteEmulatorEffs effs) => POSIXTime -> Sem effs ()
awaitTime t = do
  slotConfig <- asks pSlotConfig
  awaitSlot (posixTimeToEnclosingSlot slotConfig t + 1)

-- | Query the unspent transaction outputs at the given address.
utxosAt :: (Members ConcreteEmulatorEffs effs) => CardanoAddress -> Sem effs UtxoIndex
utxosAt addr = do
  es <- get
  pure $ C.UTxO $ Map.map (toCtxUTxOTxOut . snd) $ es ^. esAddressMap . AM.fundsAt addr

-- | Query the unspent transaction outputs at the given address (using Plutus types).
utxosAtPlutus :: (Members ConcreteEmulatorEffs effs) => CardanoAddress -> Sem effs (Map TxOutRef DecoratedTxOut)
utxosAtPlutus addr = do
  es <- get
  pure $
    Map.mapKeys fromCardanoTxIn $
      Map.mapMaybe (toDecoratedTxOut . snd) $
        es ^. esAddressMap . AM.fundsAt addr

-- | Query the unspent transaction outputs at the given transaction inputs.
utxosAtTxIns :: (Members ConcreteEmulatorEffs effs, Foldable f) => f C.TxIn -> Sem effs UtxoIndex
utxosAtTxIns txIns = do
  idx <- gets $ view $ esChainState . E.index
  pure $ foldMap (\txIn -> maybe mempty (Index.singleton txIn) $ Index.lookupUTxO txIn idx) txIns

-- | Resolve the transaction input.
utxoAtTxIn :: (Members ConcreteEmulatorEffs effs) => C.TxIn -> Sem effs (Maybe TxOut)
utxoAtTxIn txIn = gets $ Index.lookup txIn . view (esChainState . E.index)

-- | Resolve the transaction output reference (using Plutus types).
utxoAtTxOutRef :: (Members ConcreteEmulatorEffs effs) => TxOutRef -> Sem effs (Maybe DecoratedTxOut)
utxoAtTxOutRef ref = either (const $ pure Nothing) findTxOut (toCardanoTxIn ref)
  where
    findTxOut txIn = do
      mTxOut <- utxoAtTxIn txIn
      pure $ mTxOut >>= toDecoratedTxOut

-- | Query the total value of the unspent transaction outputs at the given address.
fundsAt :: (Members ConcreteEmulatorEffs effs) => CardanoAddress -> Sem effs C.Value
fundsAt addr = foldMap cardanoTxOutValue . C.unUTxO <$> utxosAt addr

-- | Resolve a datum hash to an actual datum, if known.
lookupDatum :: (Members ConcreteEmulatorEffs effs) => DatumHash -> Sem effs (Maybe Datum)
lookupDatum h = do
  es <- get
  pure $ Map.lookup h (es ^. esDatumMap)

-- | Get the internal ledger state.
getLedgerState :: (Members ConcreteEmulatorEffs effs) => Sem effs (LedgerState EmulatorEra)
getLedgerState = do
  es <- get
  pure $ es ^. esChainState . E.ledgerState . elsLedgerStateL

-- | Get the internal mempool environment.
getMemPoolEnv :: (Members ConcreteEmulatorEffs effs) => Sem effs (MempoolEnv EmulatorEra)
getMemPoolEnv = do
  es <- get
  pure $ es ^. esChainState . E.ledgerState . elsLedgerEnvL

-- | Sign a transaction with the given signatures.
signTx ::
  (Members ConcreteEmulatorEffs effs, Foldable f) =>
  -- | Signatures
  f C.ShelleyWitnessSigningKey ->
  CardanoTx ->
  Sem effs CardanoTx
signTx witnesses tx = do
  logMsg Info $ TxBalanceMsg $ SigningTx tx
  pure $ foldr addCardanoTxWitness tx witnesses

-- | Log any message
logMsg :: (Members ConcreteEmulatorEffs effs) => LogLevel -> EmulatorMsg -> Sem effs ()
logMsg l = tell . pure . LogMessage l

-- | Log a message at the 'Debug' level
logDebug :: (Members ConcreteEmulatorEffs effs, ToJSON a) => a -> Sem effs ()
logDebug = logMsg Debug . GenericMsg . toJSON

-- | Log a message at the 'Info' level
logInfo :: (Members ConcreteEmulatorEffs effs, ToJSON a) => a -> Sem effs ()
logInfo = logMsg Info . GenericMsg . toJSON

-- | Log a message at the 'Warning' level
logWarn :: (Members ConcreteEmulatorEffs effs, ToJSON a) => a -> Sem effs ()
logWarn = logMsg Warning . GenericMsg . toJSON

-- | Log a message at the 'Error' level
logError :: (Members ConcreteEmulatorEffs effs, ToJSON a) => a -> Sem effs ()
logError = logMsg Error . GenericMsg . toJSON
