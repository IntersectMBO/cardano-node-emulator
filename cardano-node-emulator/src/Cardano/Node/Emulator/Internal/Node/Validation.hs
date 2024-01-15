{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

-- | Transaction validation using 'cardano-ledger-specs'
module Cardano.Node.Emulator.Internal.Node.Validation (
  EmulatorBlock,
  EmulatedLedgerState (..),
  Coin (..),
  SlotNo (..),
  EmulatorEra,
  CardanoLedgerError,
  initialState,
  hasValidationErrors,
  createAndValidateTransactionBody,
  validateCardanoTx,
  getTxExUnitsWithLogs,
  unsafeMakeValid,
  validateAndApplyTx,

  -- * Modifying the state
  makeBlock,
  setSlot,
  nextSlot,
  UTxO (..),
  setUtxo,

  -- * Lenses
  ledgerEnv,
  memPoolState,
  currentBlock,
  previousBlocks,

  -- * Etc.
  emulatorGlobals,
) where

import Cardano.Api.Error qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.Ledger.Alonzo.Plutus.Evaluate (
  collectPlutusScriptsWithContext,
  evalPlutusScripts,
 )
import Cardano.Ledger.Alonzo.Rules (
  AlonzoUtxoPredFailure (UtxosFailure),
  AlonzoUtxosPredFailure (CollectErrors),
 )
import Cardano.Ledger.Alonzo.Tx (AlonzoTx (AlonzoTx), IsValid (IsValid))
import Cardano.Ledger.Api.Tx (
  TransactionScriptFailure (ValidationFailure),
  evalTxExUnitsWithLogs,
 )
import Cardano.Ledger.Babbage.Rules (BabbageUtxoPredFailure (AlonzoInBabbageUtxoPredFailure), BabbageUtxowPredFailure(UtxoFailure))
import Cardano.Ledger.Conway.Rules (ConwayLedgerPredFailure (ConwayUtxowFailure))
import Cardano.Ledger.BaseTypes (Globals (systemStart), epochInfo)
import Cardano.Ledger.Core qualified as Core
import Cardano.Ledger.Plutus.Evaluate (ScriptResult (Fails, Passes))
import Cardano.Ledger.Shelley.API (
  ApplyTxError (ApplyTxError),
  Coin (Coin),
  LedgerEnv (LedgerEnv, ledgerSlotNo),
  LedgerState (lsCertState, lsUTxOState),
  MempoolEnv,
  UTxO (UTxO),
  Validated,
  unsafeMakeValidated,
 )
import Cardano.Ledger.Shelley.API qualified as C.Ledger
import Cardano.Ledger.Shelley.LedgerState (LedgerState (LedgerState), smartUTxOState, utxosUtxo)
import Cardano.Node.Emulator.Internal.Node.Params (
  EmulatorEra,
  Params (emulatorPParams),
  emulatorGlobals,
  emulatorPParams,
  ledgerProtocolParameters,
 )
import Cardano.Slotting.Slot (SlotNo (SlotNo))
import Control.Lens (makeLenses, over, view, (&), (.~), (^.))
import Control.Monad.Except (MonadError (throwError))
import Data.Bifunctor (Bifunctor (first))
import Data.Default (def)
import Data.Map qualified as Map
import Data.Text qualified as Text
import Ledger.Blockchain (OnChainTx (OnChainTx))
import Ledger.Index (genesisTxIn, getCollateral)
import Ledger.Index.Internal qualified as P
import Ledger.Slot (Slot)
import Ledger.Tx (CardanoTx (CardanoEmulatorEraTx))
import Ledger.Tx.CardanoAPI qualified as P
import PlutusLedgerApi.V1 qualified as V1 hiding (TxOut (..))
import PlutusLedgerApi.V1.Scripts qualified as P

type CardanoLedgerError = Either P.ValidationErrorInPhase P.ToCardanoError

type EmulatorBlock = [Validated (Core.Tx EmulatorEra)]

{- Note [Emulated ledger]

In the real cardano node, there two types of validation: Transaction validation
(performed when a transaction is first added to the mempool) and block
validation (performed when a block is created by the local node or received
from a peer).

Transaction validation runs the Plutus scripts, checks cryptographic
signatures, balances, existence of transaction inputs and so forth. This is
where the ledger state is updated. Block validation performs other checks
related to the consensus algorithm.

Networking and consensus issues are not part of the emulator's scope. We only
care about transaction validation here, so we don't have to worry about block
validation.

The decision to leave out block validation and consensus-related concerns has
the following implications:

1. We can represent blocks as simple lists-of-transactions
2. We can modify time (the slot number) and ledger parameters as we wish,
   without having to post transactions that modify them.

There are also some limitations of the emulator's functionality that could be
addressed by extending the emulator, without having to bring in the full block
validating machinery.

\* We cannot represent different eras - everything is 'ConwayEra'.
\* There is no handling of epoch boundaries, rewards, etc.
\* The block size is unlimited - we simply take all transactions from the
  mempool when we make a block. There is however a limit on the size of
  individual transactions.
\* We use the standard ledger cryptography everywhere ('StandardCrypto').
  This could be replaced by "NoCrypto" for faster validation.

-}

-- | State of the ledger with configuration, mempool, and the blockchain.
data EmulatedLedgerState = EmulatedLedgerState
  { _ledgerEnv :: !(MempoolEnv EmulatorEra)
  , _memPoolState :: !(LedgerState EmulatorEra)
  , _currentBlock :: !EmulatorBlock
  , _previousBlocks :: ![EmulatorBlock]
  }
  deriving (Show)

makeLenses ''EmulatedLedgerState

-- | Increase the slot number by one
nextSlot :: EmulatedLedgerState -> EmulatedLedgerState
nextSlot = over ledgerEnv f
  where
    f l@LedgerEnv{ledgerSlotNo = oldSlot} = l{ledgerSlotNo = succ oldSlot}

-- | Set the slot number
setSlot :: SlotNo -> EmulatedLedgerState -> EmulatedLedgerState
setSlot sl = over ledgerEnv (\l -> l{ledgerSlotNo = sl})

-- | Set the utxo
setUtxo :: Params -> UTxO EmulatorEra -> EmulatedLedgerState -> EmulatedLedgerState
setUtxo params utxo els@EmulatedLedgerState{_memPoolState} = els{_memPoolState = newPoolState}
  where
    newPoolState =
      _memPoolState
        { lsUTxOState = smartUTxOState (emulatorPParams params) utxo (Coin 0) (Coin 0) def (Coin 0)
        }

{- | Make a block with all transactions that have been validated in the
current block, add the block to the blockchain, and empty the current block.
-}
makeBlock :: EmulatedLedgerState -> EmulatedLedgerState
makeBlock state =
  state
    & currentBlock .~ []
    & over previousBlocks ((:) (reverse $ state ^. currentBlock))

-- | Initial ledger state for a distribution
initialState :: Params -> EmulatedLedgerState
initialState params =
  EmulatedLedgerState
    { _ledgerEnv =
        C.Ledger.LedgerEnv
          { C.Ledger.ledgerSlotNo = 0
          , C.Ledger.ledgerIx = minBound
          , C.Ledger.ledgerPp = emulatorPParams params
          , C.Ledger.ledgerAccount = C.Ledger.AccountState (Coin 0) (Coin 0)
          }
    , _memPoolState =
        LedgerState
          { lsUTxOState = smartUTxOState (emulatorPParams params) (UTxO mempty) (Coin 0) (Coin 0) def (Coin 0)
          , lsCertState = def
          }
    , _currentBlock = []
    , _previousBlocks = []
    }

utxoEnv :: Params -> SlotNo -> C.Ledger.UtxoEnv EmulatorEra
utxoEnv params slotNo = C.Ledger.UtxoEnv slotNo (emulatorPParams params) def

applyTx
  :: Params
  -> EmulatedLedgerState
  -> Core.Tx EmulatorEra
  -> Either (ApplyTxError EmulatorEra) (EmulatedLedgerState, Validated (Core.Tx EmulatorEra))
applyTx params oldState@EmulatedLedgerState{_ledgerEnv, _memPoolState} tx = do
  (newMempool, vtx) <- C.Ledger.applyTx (emulatorGlobals params) _ledgerEnv _memPoolState tx
  return (oldState & memPoolState .~ newMempool & over currentBlock ((:) vtx), vtx)

hasValidationErrors :: Params -> SlotNo -> P.UtxoIndex -> C.Tx C.ConwayEra -> P.ValidationResult
hasValidationErrors params slotNo utxoIndex tx =
  case res of
    Left err -> P.FailPhase1 (CardanoEmulatorEraTx tx) err
    Right vtx -> case getTxExUnitsWithLogs params utxo tx of
      Left (P.Phase1, err) -> P.FailPhase1 (CardanoEmulatorEraTx tx) err
      Left (P.Phase2, err) -> P.FailPhase2 vtx err $ getCollateral utxoIndex (CardanoEmulatorEraTx tx)
      Right report -> P.Success vtx report
  where
    utxo = P.fromPlutusIndex utxoIndex
    res =
      OnChainTx
        <$> first
          (P.CardanoLedgerValidationError . Text.pack . show)
          (validateAndApplyTx params slotNo utxo tx)

validateAndApplyTx
  :: Params
  -> SlotNo
  -> UTxO EmulatorEra
  -> C.Tx C.ConwayEra
  -> Either (ApplyTxError EmulatorEra) (Validated (Core.Tx EmulatorEra))
validateAndApplyTx params slotNo utxo (C.ShelleyTx _ tx) = res
  where
    state = setSlot slotNo $ setUtxo params utxo $ initialState params
    res = do
      vtx <-
        constructValidated
          (emulatorGlobals params)
          (utxoEnv params slotNo)
          (lsUTxOState (_memPoolState state))
          tx
      snd <$> applyTx params state vtx

{- | Construct a 'AlonzoTx' from a 'Core.Tx' by setting the `IsValid`
flag.

Note that this simply constructs the transaction; it does not validate
anything other than the scripts. Thus the resulting transaction may be
completely invalid.

Copied from cardano-ledger as it was removed there
in https://github.com/input-output-hk/cardano-ledger/commit/721adb55b39885847562437a6fe7e998f8e48c03
-}
constructValidated
  :: forall m
   . (MonadError (ApplyTxError EmulatorEra) m)
  => Globals
  -> C.Ledger.UtxoEnv EmulatorEra
  -> C.Ledger.UTxOState EmulatorEra
  -> Core.Tx EmulatorEra
  -> m (AlonzoTx EmulatorEra)
constructValidated globals (C.Ledger.UtxoEnv _ pp _) st tx =
  case collectPlutusScriptsWithContext ei sysS pp tx utxo of
    Left errs ->
      throwError
        ( ApplyTxError
            [ConwayUtxowFailure (UtxoFailure (AlonzoInBabbageUtxoPredFailure (UtxosFailure (CollectErrors errs))))]
        )
    Right sLst ->
      let scriptEvalResult = evalPlutusScripts @EmulatorEra tx sLst
          vTx =
            AlonzoTx
              (view Core.bodyTxL tx)
              (view Core.witsTxL tx)
              (IsValid (lift scriptEvalResult))
              (view Core.auxDataTxL tx)
       in pure vTx
  where
    utxo = utxosUtxo st
    sysS = systemStart globals
    ei = epochInfo globals
    lift (Passes _) = True
    lift (Fails _ _) = False

unsafeMakeValid :: CardanoTx -> OnChainTx
unsafeMakeValid (CardanoEmulatorEraTx (C.Tx txBody _)) =
  let C.ShelleyTxBody _ txBody' _ _ _ _ = txBody
      vtx :: Core.Tx EmulatorEra = AlonzoTx txBody' mempty (IsValid True) C.Ledger.SNothing
   in OnChainTx $ unsafeMakeValidated vtx

validateCardanoTx
  :: Params
  -> Slot
  -> P.UtxoIndex
  -> CardanoTx
  -> P.ValidationResult
validateCardanoTx params slot utxo ctx@(CardanoEmulatorEraTx tx@(C.Tx (C.TxBody bodyContent) _)) =
  if map fst (C.txIns bodyContent) == [genesisTxIn]
    then P.Success (unsafeMakeValid ctx) Map.empty
    else hasValidationErrors params (fromIntegral slot) utxo tx

getTxExUnitsWithLogs
  :: Params -> UTxO EmulatorEra -> C.Tx C.ConwayEra -> Either P.ValidationErrorInPhase P.RedeemerReport
getTxExUnitsWithLogs params utxo (C.ShelleyTx _ tx) =
  case evalTxExUnitsWithLogs (emulatorPParams params) tx utxo ei ss of
    Left e -> Left . (P.Phase1,) . P.CardanoLedgerValidationError . Text.pack . show $ e
    Right result -> traverse (either toCardanoLedgerError Right) result
  where
    eg = emulatorGlobals params
    ss = systemStart eg
    ei = epochInfo eg
    toCardanoLedgerError (ValidationFailure _ (V1.CekError ce) logs _) =
      Left (P.Phase2, P.ScriptFailure (P.EvaluationError logs ("CekEvaluationFailure: " ++ show ce)))
    toCardanoLedgerError e = Left (P.Phase2, P.CardanoLedgerValidationError $ Text.pack $ show e)

createAndValidateTransactionBody
  :: Params
  -> P.CardanoBuildTx
  -> Either CardanoLedgerError (C.TxBody C.ConwayEra)
createAndValidateTransactionBody params (P.CardanoBuildTx bodyContent) =
  let bodyContent' = bodyContent{C.txProtocolParams = C.BuildTxWith $ Just $ ledgerProtocolParameters params}
   in first (Right . P.TxBodyError . C.displayError) $
        C.createAndValidateTransactionBody C.shelleyBasedEra bodyContent'
