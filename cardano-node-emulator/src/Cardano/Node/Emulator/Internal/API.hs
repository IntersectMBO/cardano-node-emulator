{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
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
    EmulatorError (..),
    EmulatorLogs,
    EmulatorMsg (..),
    MonadEmulator,
    -- EmulatorT,
    -- EmulatorM,

    -- * Running Sem chain effects
  )
where

-- handleChain,
-- processBlock,
-- modifySlot,

import Cardano.Node.Emulator.Internal.Node (ChainState, EmulatorParams)
import Cardano.Node.Emulator.Log (LogMessage, LogMsg)
import Cardano.Node.Emulator.LogMessages (EmulatorMsg (ChainEvent, GenericMsg))
import Control.Exception (Exception)
import Control.Lens (makeLenses, (&))
import Control.Monad (void)
import Data.Map (Map)
import Data.Sequence (Seq)
import Ledger
  ( Block,
    Datum,
    DatumHash,
    Slot,
    ToCardanoError,
    ValidationErrorInPhase,
    eitherTx,
    getCardanoTxData,
  )
import Ledger.AddressMap qualified as AM
import Polysemy (Sem)
import Polysemy.Error (Error)
import Polysemy.Reader (Reader)
import Polysemy.State (State)
import Polysemy.Writer (Writer)

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

type EmulatorLogs = Seq (LogMessage EmulatorMsg)

type EmulatorEffs =
  '[ Reader EmulatorParams,
     Writer EmulatorLogs,
     State EmulatorState,
     Error EmulatorError
   ]

type MonadEmulator = Sem EmulatorEffs

-- handleChain :: (MonadEmulator m) => Sem [E.ChainControlEffect, E.ChainEffect] a -> m a
-- handleChain eff = do
--   params <- ask
--   EmulatorState chainState am dm <- get
--   let ((((a, dm'), am'), newChainState), lg) =
--         raiseEnd eff
--           & interpret (E.handleControlChain params)
--           & interpret (E.handleChain params)
--           & interpret handleChainLogs
--           & runState dm
--           & runState am
--           & runState chainState
--           & F.runWriter
--           & run
--   tell lg
--   put $ EmulatorState newChainState am' dm'
--   pure a
--   where
--     handleChainLogs ::
--       ( Member (State AM.AddressMap) effs,
--         Member (State (Map DatumHash Datum)) effs,
--         Member (F.Writer EmulatorLogs) effs
--       ) =>
--       LogMsg E.ChainEvent ~> Sem effs
--     handleChainLogs (LMessage msg@(LogMessage _ e)) = do
--       F.tell @EmulatorLogs (pure $ ChainEvent <$> msg)
--       E.chainEventOnChainTx e
--         & maybe
--           (pure ())
--           ( \tx -> do
--               void $ modify $ AM.updateAllAddresses tx
--               void $ modify $ ((<>) . eitherTx getCardanoTxData getCardanoTxData) tx
--           )

-- processBlock :: (MonadEmulator m) => m Block
-- processBlock = handleChain E.processBlock

-- modifySlot :: (MonadEmulator m) => (Slot -> Slot) -> m Slot
-- modifySlot f = handleChain $ E.modifySlot f
