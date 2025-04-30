{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

-- | Transaction validation using 'cardano-ledger-specs'
module Cardano.Node.Emulator.Internal.Node.Validation
  ( EmulatedLedgerState (..),
    initialState,
    hasValidationErrors,
    createTransactionBody,
    validateCardanoTx,
    getTxExUnitsWithLogs,
    unsafeMakeValid,
    validateAndApplyTx,
    nextSlot,
    elsLedgerEnvL,
    elsLedgerStateL,
    elsUtxoL,
    elsSlotL,
    getSlot,
    updateStateParams,
    elsConstitutionScriptL,
    registerStakeCredential,
  )
where

import Cardano.Api.Internal.Error qualified as C.Api
import Cardano.Api.Shelley qualified as C.Api
import Cardano.Ledger.Alonzo.Plutus.Evaluate qualified as Alonzo
import Cardano.Ledger.Alonzo.Rules qualified as Alonzo
import Cardano.Ledger.Alonzo.Tx qualified as Alonzo
import Cardano.Ledger.Coin qualified as Cardano
import Cardano.Ledger.Conway.Governance qualified as Conway
import Cardano.Ledger.Conway.Rules (ConwayLedgerPredFailure (ConwayUtxowFailure))
import Cardano.Ledger.Core qualified as Ledger
import Cardano.Ledger.Credential qualified as Cardano
import Cardano.Ledger.Plutus.Evaluate qualified as Ledger
import Cardano.Ledger.Shelley.API qualified as Shelley
import Cardano.Ledger.Shelley.LedgerState qualified as Shelley
import Cardano.Ledger.Shelley.Rules qualified as Shelley
import Cardano.Ledger.Shelley.Transition qualified as Shelley
import Cardano.Ledger.UMap qualified as UM
import Cardano.Node.Emulator.Internal.Node.Params
  ( EmulatorEra,
    Params (pConfig),
    emulatorGlobals,
    emulatorPParams,
    ledgerProtocolParameters,
  )
import Control.Lens (Lens', makeLensesFor, over, view, (^.))
import Control.Monad.Except (MonadError (throwError))
import Data.Bifunctor (first)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map qualified as Map
import Data.Text qualified as Text
import Ledger qualified as P.Ledger
import Ledger.Tx.CardanoAPI qualified as P.Ledger
import PlutusLedgerApi.V1 qualified as PV1

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
  { elsLedgerEnv :: !(Shelley.LedgerEnv EmulatorEra),
    elsLedgerState :: !(Shelley.LedgerState EmulatorEra)
  }
  deriving (Show)

makeLensesFor
  [ ("elsLedgerEnv", "elsLedgerEnvL"),
    ("elsLedgerState", "elsLedgerStateL")
  ]
  ''EmulatedLedgerState

registerStakeCredential :: Cardano.StakeCredential -> Cardano.Coin -> Cardano.Coin -> EmulatedLedgerState -> EmulatedLedgerState
registerStakeCredential sCred reward deposit =
  over
    ( elsLedgerStateL
        . Shelley.lsCertStateL
        . Shelley.certDStateL
        . Shelley.dsUnifiedL
    )
    ( UM.insert
        sCred
        ( UM.RDPair
            (UM.compactCoinOrError reward)
            (UM.compactCoinOrError deposit)
        )
        . UM.RewDepUView
    )

-- | Tampers with the Slot number
elsSlotL :: Lens' EmulatedLedgerState C.Api.SlotNo
elsSlotL = elsLedgerEnvL . Shelley.ledgerSlotNoL

-- | Increases the slot number by one
nextSlot :: EmulatedLedgerState -> EmulatedLedgerState
nextSlot = over elsSlotL succ

-- | Get the slot number as any 'Num'
getSlot :: (Num a) => EmulatedLedgerState -> a
getSlot = fromIntegral . C.Api.unSlotNo . view elsSlotL

-- | Tampers with the index
elsUtxoL :: Lens' EmulatedLedgerState (Shelley.UTxO EmulatorEra)
elsUtxoL = elsLedgerStateL . Shelley.utxoL

-- | Tampers with the constitution script
elsConstitutionScriptL :: Lens' EmulatedLedgerState (Shelley.StrictMaybe Shelley.ScriptHash)
elsConstitutionScriptL =
  elsLedgerStateL
    . Shelley.lsUTxOStateL
    . Shelley.utxosGovStateL
    . Conway.constitutionGovStateL
    . Conway.constitutionScriptL

-- | Initial ledger state for a distribution
initialState :: Params -> EmulatedLedgerState
initialState params =
  EmulatedLedgerState
    { elsLedgerEnv =
        Shelley.LedgerEnv
          { Shelley.ledgerSlotNo = 0,
            Shelley.ledgerIx = minBound,
            Shelley.ledgerPp = emulatorPParams params,
            Shelley.ledgerAccount = Shelley.AccountState (Shelley.Coin 0) (Shelley.Coin 0),
            Shelley.ledgerEpochNo = Nothing
          },
      elsLedgerState = Shelley.esLState (Shelley.nesEs (Shelley.createInitialState (pConfig params)))
    }

-- | This updates an 'EmulatedLedgerState' with new params. For now, we only
-- update the 'Shelley.ledgerPp' part and not the 'elsLedgerState' because they
-- are only related to the genesis of the state.
updateStateParams :: Params -> EmulatedLedgerState -> EmulatedLedgerState
updateStateParams params (EmulatedLedgerState ledgerEnv ledgerState) =
  EmulatedLedgerState (ledgerEnv {Shelley.ledgerPp = emulatorPParams params}) ledgerState

applyTx ::
  Params ->
  EmulatedLedgerState ->
  Ledger.Tx EmulatorEra ->
  Either (Shelley.ApplyTxError EmulatorEra) (EmulatedLedgerState, Shelley.Validated (Ledger.Tx EmulatorEra))
applyTx params (EmulatedLedgerState ledgerEnv ledgerState) tx =
  first (EmulatedLedgerState ledgerEnv) <$> Shelley.applyTx (emulatorGlobals params) ledgerEnv ledgerState tx

hasValidationErrors ::
  Params ->
  EmulatedLedgerState ->
  C.Api.Tx C.Api.ConwayEra ->
  (EmulatedLedgerState, P.Ledger.ValidationResult)
hasValidationErrors params ls tx =
  case validateAndApplyTx params ls tx of
    -- 'validateAndApplyTx' will only consider phase 1 failures as
    -- errors. Indeed, fail 2 failures will still lead to successes where the
    -- collaterals will be inserted into the index.
    Left err -> (ls, P.Ledger.FailPhase1 (P.Ledger.CardanoEmulatorEraTx tx) (P.Ledger.CardanoLedgerValidationError . Text.pack . show $ err))
    -- We need to catch phase 2 failures, though, which is why we call
    -- 'getTxExUnitsWithLogs' which is going to throw all phase 2 failures,
    -- except for those revolving around execution units.
    Right (ls', P.Ledger.OnChainTx -> vtx) -> case getTxExUnitsWithLogs params (ls ^. elsUtxoL) tx of
      -- This case is unreachable because Phase 1 failures are already caught by
      -- 'validateAndApplyTx'
      Left (P.Ledger.Phase1, err) -> (ls', P.Ledger.FailPhase1 (P.Ledger.CardanoEmulatorEraTx tx) err)
      -- This case corresponds to all Phase 2 failures except those revolving
      -- around execution units.
      Left (P.Ledger.Phase2, err) -> (ls', P.Ledger.FailPhase2 vtx err $ P.Ledger.getCollateral (P.Ledger.toPlutusIndex $ ls ^. elsUtxoL) (P.Ledger.CardanoEmulatorEraTx tx))
      -- To handle the execution units failures, we inspect the validated
      -- transaction and check if we get a @IsValid False@.
      Right _
        | Alonzo.IsValid False <- Alonzo.isValid $ Shelley.extractTx $ P.Ledger.getOnChainTx vtx ->
            ( ls',
              P.Ledger.FailPhase2
                vtx
                (P.Ledger.CardanoLedgerValidationError "Insufficient execution units provided.")
                (P.Ledger.getCollateral (P.Ledger.toPlutusIndex $ ls ^. elsUtxoL) (P.Ledger.CardanoEmulatorEraTx tx))
            )
      Right report -> (ls', P.Ledger.Success vtx report)

validateAndApplyTx ::
  Params ->
  EmulatedLedgerState ->
  C.Api.Tx C.Api.ConwayEra ->
  Either
    (Shelley.ApplyTxError EmulatorEra)
    (EmulatedLedgerState, Shelley.Validated (Ledger.Tx EmulatorEra))
validateAndApplyTx params ledgerState (C.Api.ShelleyTx _ tx) =
  constructValidated
    (emulatorGlobals params)
    ( Shelley.UtxoEnv
        (ledgerState ^. elsSlotL)
        (emulatorPParams params)
        (Shelley.lsCertState $ ledgerState ^. elsLedgerStateL)
    )
    (Shelley.lsUTxOState $ ledgerState ^. elsLedgerStateL)
    tx
    >>= applyTx params ledgerState

-- | Construct a 'AlonzoTx' from a 'Core.Tx' by setting the `IsValid`
-- flag.
--
-- Note that this simply constructs the transaction; it does not validate
-- anything other than the scripts. Thus the resulting transaction may be
-- completely invalid.
--
-- Copied from cardano-ledger as it was removed there
-- in https://github.com/input-output-hk/cardano-ledger/commit/721adb55b39885847562437a6fe7e998f8e48c03
constructValidated ::
  forall m.
  (MonadError (Shelley.ApplyTxError EmulatorEra) m) =>
  Shelley.Globals ->
  Shelley.UtxoEnv EmulatorEra ->
  Shelley.UTxOState EmulatorEra ->
  Ledger.Tx EmulatorEra ->
  m (Alonzo.AlonzoTx EmulatorEra)
constructValidated globals (Shelley.UtxoEnv _ pp _) st tx =
  case Alonzo.collectPlutusScriptsWithContext ei sysS pp tx utxo of
    Left errs ->
      throwError
        ( Shelley.ApplyTxError
            ( ConwayUtxowFailure
                (Ledger.injectFailure (Alonzo.UtxosFailure (Ledger.injectFailure $ Alonzo.CollectErrors errs)))
                :| []
            )
        )
    Right sLst ->
      let scriptEvalResult = Alonzo.evalPlutusScripts sLst
          vTx =
            Alonzo.AlonzoTx
              (view Ledger.bodyTxL tx)
              (view Ledger.witsTxL tx)
              (Alonzo.IsValid (lift scriptEvalResult))
              (view Ledger.auxDataTxL tx)
       in pure vTx
  where
    utxo = Shelley.utxosUtxo st
    sysS = Shelley.systemStart globals
    ei = Shelley.epochInfo globals
    lift (Ledger.Passes _) = True
    lift (Ledger.Fails _ _) = False

unsafeMakeValid :: P.Ledger.CardanoTx -> P.Ledger.OnChainTx
unsafeMakeValid (P.Ledger.CardanoEmulatorEraTx (C.Api.Tx txBody _)) =
  let C.Api.ShelleyTxBody _ txBody' _ _ _ _ = txBody
      vtx :: Ledger.Tx EmulatorEra = Alonzo.AlonzoTx txBody' mempty (Alonzo.IsValid True) Shelley.SNothing
   in P.Ledger.OnChainTx $ Shelley.unsafeMakeValidated vtx

validateCardanoTx ::
  Params ->
  EmulatedLedgerState ->
  P.Ledger.CardanoTx ->
  (EmulatedLedgerState, P.Ledger.ValidationResult)
validateCardanoTx params ls ctx@(P.Ledger.CardanoEmulatorEraTx tx) =
  if map fst (C.Api.txIns $ C.Api.getTxBodyContent $ C.Api.getTxBody tx) == [P.Ledger.genesisTxIn]
    then (ls, P.Ledger.Success (unsafeMakeValid ctx) Map.empty)
    else hasValidationErrors params ls tx

getTxExUnitsWithLogs ::
  Params ->
  Shelley.UTxO EmulatorEra ->
  C.Api.Tx C.Api.ConwayEra ->
  Either P.Ledger.ValidationErrorInPhase P.Ledger.RedeemerReport
getTxExUnitsWithLogs params utxo (C.Api.ShelleyTx _ tx) =
  traverse (either toCardanoLedgerError Right) result
  where
    eg = emulatorGlobals params
    result = Alonzo.evalTxExUnitsWithLogs (emulatorPParams params) tx utxo (Shelley.epochInfo eg) (Shelley.systemStart eg)
    toCardanoLedgerError (Alonzo.ValidationFailure _ (PV1.CekError ce) logs _) =
      Left (P.Ledger.Phase2, P.Ledger.ScriptFailure (PV1.EvaluationError logs ("CekEvaluationFailure: " ++ show ce)))
    toCardanoLedgerError e = Left (P.Ledger.Phase2, P.Ledger.CardanoLedgerValidationError $ Text.pack $ show e)

createTransactionBody ::
  Params ->
  P.Ledger.CardanoBuildTx ->
  Either P.Ledger.ToCardanoError (C.Api.TxBody C.Api.ConwayEra)
createTransactionBody params (P.Ledger.CardanoBuildTx bodyContent) =
  let bodyContent' = bodyContent {C.Api.txProtocolParams = C.Api.BuildTxWith $ Just $ ledgerProtocolParameters params}
   in first (P.Ledger.TxBodyError . C.Api.displayError) $
        C.Api.createTransactionBody C.Api.shelleyBasedEra bodyContent'
