{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Node.Emulator.Internal.Node.Chain where

import Cardano.Api (SlotNo (SlotNo))
import Cardano.Node.Emulator.Internal.Node.Params (EmulatorParams)
import Cardano.Node.Emulator.Internal.Node.Validation qualified as Validation
import Cardano.Node.Emulator.Log (LogMsg, logDebug, logInfo, logWarn)
import Data.Aeson (FromJSON, ToJSON)
import Data.Foldable (traverse_)
import Data.List ((\\))
import Data.Maybe (mapMaybe)
import Data.Traversable (for)
import GHC.Generics (Generic)
import Ledger
  ( Block,
    Blockchain,
    CardanoTx,
    OnChainTx,
    Slot,
    getCardanoTxId,
    unOnChain,
  )
import Ledger.Index qualified as Index
import Optics.Core (over, view, (%), (%~), (&), (.~))
import Optics.TH (makeLenses, makePrisms)
import Polysemy
  ( Effect,
    InterpreterFor,
    Member,
    Members,
    Sem,
    interpret,
    makeSem,
    run,
  )
import Polysemy.State (State, get, gets, modify, put, runState)
import Prettyprinter (Pretty (pretty), vsep, (<+>))

-- | Events produced by the blockchain emulator.
data ChainEvent
  = -- | A transaction has been validated and added to the blockchain.
    TxnValidation !Index.ValidationResult
  | SlotAdd !Slot
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Pretty ChainEvent where
  pretty = \case
    TxnValidation res ->
      vsep
        ["TxnValidation" <+> pretty (getCardanoTxId $ Index.cardanoTxFromValidationResult res), pretty res]
    SlotAdd sl -> "SlotAdd" <+> pretty sl

chainEventOnChainTx :: ChainEvent -> Maybe OnChainTx
chainEventOnChainTx (TxnValidation result) = Index.toOnChain result
chainEventOnChainTx _ = Nothing

-- | A pool of transactions which have yet to be validated.
type TxPool = [CardanoTx]

data ChainState = ChainState
  { -- | The current chain, with the newest transactions first in the list.
    _chainNewestFirst :: !Blockchain,
    -- | The pool of pending transactions.
    _txPool :: !TxPool,
    -- | The UTxO index, used for validation.
    _index :: !Index.UtxoIndex,
    -- | The internal state of the ledger.
    _ledgerState :: Validation.EmulatedLedgerState
  }
  deriving (Show, Generic)

makeLenses ''ChainState

emptyChainState :: EmulatorParams -> ChainState
emptyChainState params = ChainState [] [] mempty (Validation.initialState params)

fromBlockchain :: EmulatorParams -> Blockchain -> ChainState
fromBlockchain params bc =
  emptyChainState params
    & chainNewestFirst
    .~ bc
    & index
    .~ Index.initialise bc

data ChainControlEffect :: Effect where
  ProcessBlock :: ChainControlEffect m Block
  ModifySlot :: (Slot -> Slot) -> ChainControlEffect m Slot

makeSem ''ChainControlEffect

data ChainEffect :: Effect where
  QueueTx :: CardanoTx -> ChainEffect m ()
  GetCurrentSlot :: ChainEffect m Slot
  GetParams :: ChainEffect m EmulatorParams

makeSem ''ChainEffect

handleControlChain ::
  (Members [State ChainState, LogMsg ChainEvent] effs) =>
  EmulatorParams ->
  InterpreterFor ChainControlEffect effs
handleControlChain params = interpret $ \case
  ProcessBlock -> do
    pool <- gets $ view txPool
    idx <- gets $ view index
    ls <- gets $ view ledgerState

    let ValidatedBlock block events idx' ls' =
          validateBlock params idx ls pool

    modify $ txPool .~ []
    modify $ index .~ idx'
    modify $ ledgerState .~ ls'
    modify $ addBlock block

    traverse_ logChainEvent events
    pure block
  ModifySlot f -> do
    modify @ChainState
      ( over
          (ledgerState % Validation.elsSlotL)
          (\(SlotNo s) -> fromIntegral (f (fromIntegral s)))
      )
    gets (Validation.getSlot . view ledgerState)

logChainEvent :: (Member (LogMsg ChainEvent) effs) => ChainEvent -> Sem effs ()
logChainEvent e = case e of
  SlotAdd {} -> logDebug e
  TxnValidation Index.FailPhase1 {} -> logWarn e
  TxnValidation Index.FailPhase2 {} -> logWarn e
  TxnValidation Index.Success {} -> logInfo e

handleChain ::
  (Members [State ChainState, LogMsg ChainEvent] effs) =>
  EmulatorParams ->
  InterpreterFor ChainEffect effs
handleChain params = interpret $ \case
  QueueTx tx -> modify (addTxToPool tx)
  GetCurrentSlot -> gets (Validation.getSlot . view ledgerState)
  GetParams -> pure params

-- | The result of validating a block.
data ValidatedBlock = ValidatedBlock
  { -- | The transactions that have been validated in this block.
    vlbValid :: !Block,
    -- | Transaction validation events for the transactions in this block.
    vlbEvents :: ![ChainEvent],
    -- | The updated UTxO index after processing the block
    vlbIndex :: !Index.UtxoIndex,
    vlbLedgerState :: !Validation.EmulatedLedgerState
  }

data ValidationCtx = ValidationCtx
  { vctxIndex :: !Index.UtxoIndex,
    vctxParams :: !EmulatorParams,
    vctxLedgerState :: Validation.EmulatedLedgerState
  }

-- | Validate a block given the current slot and UTxO index, returning the valid
-- transactions, success/failure events and the updated UTxO set.
validateBlock ::
  EmulatorParams -> Index.UtxoIndex -> Validation.EmulatedLedgerState -> TxPool -> ValidatedBlock
validateBlock params idx ls txns =
  let -- Validate transactions, updating the UTXO index each time
      (ValidationCtx idx' _ ls', results) = run $ runState (ValidationCtx idx params ls) $ for txns validateEm

      -- The new block contains all transaction that were validated
      -- successfully
      block = mapMaybe Index.toOnChain results

      -- Also return an `EmulatorEvent` for each transaction that was
      -- processed
      events = (TxnValidation <$> results) ++ [SlotAdd $ Validation.getSlot ls + 1]
   in ValidatedBlock block events idx' ls'

-- | Validate a transaction in the current emulator state.
validateEm ::
  (Member (State ValidationCtx) effs) =>
  CardanoTx ->
  Sem effs Index.ValidationResult
validateEm txn = do
  ctx@(ValidationCtx idx params ls) <- get
  let (ls', res) = Validation.validateCardanoTx params ls txn
      idx' = case res of
        Index.FailPhase1 {} -> idx
        Index.FailPhase2 {} -> Index.insertCollateral txn idx
        Index.Success {} -> Index.insert txn idx
  _ <- put ctx {vctxIndex = idx', vctxLedgerState = ls'}
  pure res

-- | Adds a block to ChainState, without validation.
addBlock :: Block -> ChainState -> ChainState
addBlock blk st =
  st
    & chainNewestFirst
    %~ (blk :)
    -- The block update may contain txs that are not in this client's
    -- `txPool` which will get ignored
    & txPool
    %~ (\\ map unOnChain blk)

addTxToPool :: CardanoTx -> ChainState -> ChainState
addTxToPool tx = over txPool (tx :)

makePrisms ''ChainEvent
