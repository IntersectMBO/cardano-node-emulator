{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | If you want to run the node emulator without using the `Contract` monad,
-- this module provides a simple polysemy-based interface.
module Cardano.Node.Emulator.Internal.API
  ( -- * Types
    EmulatorState (EmulatorState),
    esChainState,
    esAddressMap,
    esDatumMap,
    ConcreteEmulatorEffs,
    EmulatorError (..),
    EmulatorLogs,
    EmulatorMsg (..),
    processBlock,
    modifySlot,

    -- * Running Sem chain effects
    handleChainLogs,
    handleChainEffects,
  )
where

import Cardano.Node.Emulator.Internal.Node (ChainState, EmulatorParams)
import Cardano.Node.Emulator.Internal.Node.Chain
  ( ChainControlEffect,
    ChainEffect,
    ChainEvent,
    chainEventOnChainTx,
    handleChain,
    handleControlChain,
  )
import Cardano.Node.Emulator.Internal.Node.Chain qualified as C
import Cardano.Node.Emulator.Log (LogMessage (LogMessage), LogMsg (LogMsg))
import Cardano.Node.Emulator.LogMessages (EmulatorMsg (ChainEvent))
import Data.Map (Map)
import Data.Sequence (Seq)
import GHC.Exception (Exception)
import Ledger
  ( Datum,
    DatumHash,
    ToCardanoError,
    ValidationErrorInPhase,
    eitherTx,
    getCardanoTxData,
  )
import Ledger.AddressMap qualified as AM
import Ledger.Blockchain (Block)
import Ledger.Slot (Slot)
import Optics.Core ((&))
import Optics.TH (makeLenses)
import Polysemy (InterpreterFor, Member, Members, Sem, insertAt, interpret, run)
import Polysemy.Error (Error)
import Polysemy.Reader (Reader, ask)
import Polysemy.State (State, get, modify, put, runState)
import Polysemy.Writer (Writer, runWriter, tell)

data EmulatorState = EmulatorState
  { _esChainState :: !ChainState,
    _esAddressMap :: !AM.AddressMap,
    _esDatumMap :: !(Map DatumHash Datum)
  }
  deriving (Show)

makeLenses 'EmulatorState

data EmulatorError
  = ValidationError !ValidationErrorInPhase
  | ToCardanoError !ToCardanoError
  | CustomError !String
  deriving (Show)

deriving instance Exception EmulatorError

type EmulatorLogs = Seq (LogMessage EmulatorMsg)

type ConcreteEmulatorEffs =
  '[ Error EmulatorError,
     Reader EmulatorParams,
     Writer EmulatorLogs,
     State EmulatorState
   ]

handleChainLogs ::
  ( Member (State AM.AddressMap) effs,
    Member (State (Map DatumHash Datum)) effs,
    Member (Writer EmulatorLogs) effs
  ) =>
  InterpreterFor (LogMsg ChainEvent) effs
handleChainLogs = interpret $ \(LogMsg msg@(LogMessage _ e)) -> do
  tell $ pure $ ChainEvent <$> msg
  chainEventOnChainTx
    e
    & maybe
      (pure ())
      ( \tx -> do
          modify $ AM.updateAllAddresses tx
          modify $ ((<>) . eitherTx getCardanoTxData getCardanoTxData) tx
      )

-- Handle both chain effects on top of an arbitray stack.
handleChainEffects ::
  (Members ConcreteEmulatorEffs effs) =>
  Sem [ChainControlEffect, ChainEffect] a ->
  Sem effs a
handleChainEffects comp = do
  params <- ask
  EmulatorState chainState addressMap datumMap <- get
  let (emulatorLogs, (chainState', (addressMap', (datumMap', v)))) =
        comp
          & insertAt @2
            @[ LogMsg ChainEvent,
               State (Map DatumHash Datum),
               State AM.AddressMap,
               State ChainState,
               Writer EmulatorLogs
             ]
          & handleControlChain params
          & handleChain params
          & handleChainLogs
          & runState datumMap
          & runState addressMap
          & runState chainState
          & runWriter
          & run
  tell emulatorLogs
  put $ EmulatorState chainState' addressMap' datumMap'
  pure v

processBlock :: (Members ConcreteEmulatorEffs effs) => Sem effs Block
processBlock = handleChainEffects C.processBlock

modifySlot :: (Members ConcreteEmulatorEffs effs) => (Slot -> Slot) -> Sem effs Slot
modifySlot = handleChainEffects . C.modifySlot
