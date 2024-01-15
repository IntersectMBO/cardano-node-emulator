{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

-- | Calculating transaction fees in the emulator.
module Cardano.Node.Emulator.Internal.Node.Fee (
  estimateCardanoBuildTxFee,
  fillTxExUnits,
  makeAutoBalancedTransaction,
  makeAutoBalancedTransactionWithUtxoProvider,
  utxoProviderFromWalletOutputs,
  BalancingError (..),

  -- * Internals
  selectCoin,
) where

import Cardano.Api qualified as C
import Cardano.Api.Error qualified as C.Api
import Cardano.Api.Fees (mapTxScriptWitnesses)
import Cardano.Api.Shelley qualified as C
import Cardano.Api.Shelley qualified as C.Api
import Cardano.Ledger.BaseTypes (Globals (systemStart), epochInfo)
import Cardano.Node.Emulator.Internal.Node.Params (
  EmulatorEra,
  Params (emulatorPParams),
  emulatorGlobals,
  ledgerProtocolParameters,
  pProtocolParams,
 )
import Cardano.Node.Emulator.Internal.Node.Validation (
  CardanoLedgerError,
  UTxO (UTxO),
  createAndValidateTransactionBody,
  getTxExUnitsWithLogs,
 )
import Control.Arrow ((&&&))
import Control.Lens (over, (&))
import Data.Aeson (FromJSON, ToJSON)
import Data.Bifunctor (bimap, first)
import Data.Foldable (fold, foldl', toList)
import Data.List (sortOn, (\\))
import Data.Map qualified as Map
import Data.Maybe (isNothing, listToMaybe)
import Data.Ord (Down (Down))
import GHC.Generics (Generic)
import Ledger.Address (CardanoAddress)
import Ledger.Index (
  UtxoIndex,
  ValidationError (MaxCollateralInputsExceeded, TxOutRefNotFound),
  ValidationPhase (Phase1),
  adjustTxOut,
  minAdaTxOutEstimated,
 )
import Ledger.Tx (ToCardanoError (TxBodyError), TxOut)
import Ledger.Tx qualified as Tx
import Ledger.Tx.CardanoAPI (
  CardanoBuildTx (CardanoBuildTx),
  getCardanoBuildTx,
  toCardanoFee,
  toCardanoReturnCollateral,
  toCardanoTotalCollateral,
 )
import Ledger.Tx.CardanoAPI qualified as CardanoAPI
import Ledger.Value.CardanoAPI (isZero, lovelaceToValue, split, valueGeq)

estimateCardanoBuildTxFee
  :: Params
  -> CardanoBuildTx
  -> Either CardanoLedgerError C.Lovelace
estimateCardanoBuildTxFee params txBodyContent = do
  let nkeys = C.Api.estimateTransactionKeyWitnessCount (getCardanoBuildTx txBodyContent)
  txBody <- first Right $ CardanoAPI.createTransactionBody txBodyContent
  pure $ evaluateTransactionFee params txBody nkeys

fillTxExUnits :: Params -> UtxoIndex -> CardanoBuildTx -> Either CardanoLedgerError CardanoBuildTx
fillTxExUnits params txUtxo buildTx@(CardanoBuildTx txBodyContent) = do
  tmpTx' <-
    first Right $
      C.makeSignedTransaction [] <$> CardanoAPI.createTransactionBody buildTx
  exUnitsMap' <-
    bimap
      Left
      (Map.mapKeys (C.toScriptIndex C.AlonzoEraOnwardsBabbage) . fmap (C.fromAlonzoExUnits . snd))
      $ getTxExUnitsWithLogs params (CardanoAPI.fromPlutusIndex txUtxo) tmpTx'
  bimap (Right . TxBodyError . C.Api.displayError) CardanoBuildTx $
    mapTxScriptWitnesses (mapWitness exUnitsMap') txBodyContent
  where
    mapWitness
      :: Map.Map C.Api.ScriptWitnessIndex C.Api.ExecutionUnits
      -> C.ScriptWitnessIndex
      -> C.ScriptWitness witctx era
      -> Either (C.TxBodyErrorAutoBalance era) (C.ScriptWitness witctx era)
    mapWitness _ _ wit@C.SimpleScriptWitness{} = Right wit
    mapWitness eum idx (C.PlutusScriptWitness langInEra version script datum redeemer _) =
      case Map.lookup idx eum of
        Nothing ->
          Left $ C.TxBodyErrorScriptWitnessIndexMissingFromExecUnitsMap idx eum
        Just exunits ->
          Right $
            C.PlutusScriptWitness
              langInEra
              version
              script
              datum
              redeemer
              exunits

{- | Creates a balanced transaction by calculating the execution units, the fees and the change,
which is assigned to the given address. Only balances Ada.
-}
makeAutoBalancedTransaction
  :: Params
  -> UTxO EmulatorEra
  -- ^ Just the transaction inputs, not the entire 'UTxO'.
  -> CardanoBuildTx
  -> CardanoAddress
  -- ^ Change address
  -> Either CardanoLedgerError (C.Api.Tx C.Api.ConwayEra)
makeAutoBalancedTransaction params utxo (CardanoBuildTx txBodyContent) cChangeAddr = first Right $ do
  -- Compute the change.
  C.Api.BalancedTxBody _ _ change _ <- first (TxBodyError . C.Api.displayError) $ balance []
  let
    -- Recompute execution units with full set of UTxOs, including change.
    trial = balance [change]
    -- Correct for a negative balance in cases where execution units, and hence fees, have increased.
    change' =
      case (change, trial) of
        ( C.Api.TxOut addr txoVal datum _referenceScript
          , Left (C.Api.TxBodyErrorAdaBalanceNegative delta)
          ) ->
            C.Api.TxOut
              addr
              (CardanoAPI.toCardanoTxOutValue $ CardanoAPI.fromCardanoTxOutValue txoVal <> lovelaceToValue delta)
              datum
              _referenceScript
        _ -> change
  -- Construct the body with correct execution units and fees.
  C.Api.BalancedTxBody _ txBody _ _ <- first (TxBodyError . C.Api.displayError) $ balance [change']
  pure $ C.Api.makeSignedTransaction [] txBody
  where
    globals = emulatorGlobals params
    ei = C.Api.LedgerEpochInfo $ epochInfo globals
    ss = systemStart globals
    utxo' = fromLedgerUTxO utxo
    balance extraOuts =
      C.Api.makeTransactionBodyAutoBalance
        C.Api.shelleyBasedEra
        ss
        ei
        (ledgerProtocolParameters params)
        mempty
        mempty
        mempty
        utxo'
        txBodyContent{C.Api.txOuts = C.Api.txOuts txBodyContent ++ extraOuts}
        cChangeAddr
        Nothing

{- | A utxo provider returns outputs that cover at least the given value,
and return the change, i.e. how much the outputs overshoot the given value.
-}
type UtxoProvider m = C.Value -> m (UtxoIndex, C.Value)

{- | Creates a balanced transaction by calculating the execution units, the fees and then the balance.
If the balance is negative the utxo provider is asked to pick extra inputs to make the balance is positive,
which is then assigned to the change address.
The collateral is similarly balanced.
Unlike `makeAutoBalancedTransaction` this function also balances non-Ada.
-}
makeAutoBalancedTransactionWithUtxoProvider
  :: (Monad m)
  => Params
  -> UtxoIndex
  -- ^ Just the transaction inputs, not the entire 'UTxO'.
  -> CardanoAddress
  -- ^ Change address
  -> UtxoProvider m
  -> (forall a. CardanoLedgerError -> m a)
  -- ^ How to handle errors
  -> CardanoBuildTx
  -> m (C.Tx C.ConwayEra)
makeAutoBalancedTransactionWithUtxoProvider params txUtxo cChangeAddr utxoProvider errorReporter (CardanoBuildTx unbalancedBodyContent) = do
  let
    -- Set the params alreay since it makes the tx bigger and so influences the fee
    unbalancedBodyContent' =
      unbalancedBodyContent{C.txProtocolParams = C.BuildTxWith $ Just $ ledgerProtocolParameters params}
    initialFeeEstimate = C.Lovelace 300_000

    balanceAndFillExUnits fee = do
      (txBodyContent, utxo') <-
        handleBalanceTx params txUtxo cChangeAddr utxoProvider errorReporter fee unbalancedBodyContent'

      either errorReporter pure $ fillTxExUnits params (txUtxo <> utxo') (CardanoBuildTx txBodyContent)

    calcFee n fee = do
      buildTx <- balanceAndFillExUnits fee

      newFee <-
        either errorReporter pure $
          estimateCardanoBuildTxFee params buildTx

      if newFee /= fee
        then
          if n == (0 :: Int)
            then -- If we don't reach a fixed point, pick the larger fee
              pure (newFee `max` fee)
            else calcFee (n - 1) newFee
        else pure newFee

  theFee <- calcFee 5 initialFeeEstimate

  txBodyContent <- balanceAndFillExUnits theFee

  either errorReporter pure $ do
    C.makeSignedTransaction []
      <$> createAndValidateTransactionBody params txBodyContent

-- | Balance an unbalanced transaction by adding missing inputs and outputs
handleBalanceTx
  :: (Monad m)
  => Params
  -> UtxoIndex
  -- ^ Just the transaction inputs, not the entire 'UTxO'.
  -> C.AddressInEra C.ConwayEra
  -- ^ Change address
  -> UtxoProvider m
  -- ^ The utxo provider
  -> (forall a. CardanoLedgerError -> m a)
  -- ^ How to handle errors
  -> C.Lovelace
  -- ^ Estimated fee value to use.
  -> C.TxBodyContent C.BuildTx C.ConwayEra
  -> m (C.TxBodyContent C.BuildTx C.ConwayEra, UtxoIndex)
handleBalanceTx params (C.UTxO txUtxo) cChangeAddr utxoProvider errorReporter fees utx = do
  let theFee = toCardanoFee fees

  let filteredUnbalancedTxTx = removeEmptyOutputsBuildTx utx{C.txFee = theFee}
      txInputs = Tx.getTxBodyContentInputs filteredUnbalancedTxTx

      lookupValue txIn =
        maybe
          (errorReporter (Left (Phase1, TxOutRefNotFound txIn)))
          (pure . Tx.cardanoTxOutValue)
          (Map.lookup txIn txUtxo)

  inputValues <- traverse lookupValue txInputs

  let left = Tx.getTxBodyContentMint filteredUnbalancedTxTx <> fold inputValues
      right = lovelaceToValue fees <> foldMap (Tx.txOutValue . Tx.TxOut) (C.txOuts filteredUnbalancedTxTx)
      balance = left <> C.negateValue right

  ((neg, newInputs), (pos, mNewTxOut)) <-
    calculateTxChanges params cChangeAddr utxoProvider $ split balance

  let newTxIns = map (,C.BuildTxWith $ C.KeyWitness C.KeyWitnessForSpending) $ Map.keys $ C.unUTxO newInputs

  let txWithOutputsAdded =
        if isZero pos
          then filteredUnbalancedTxTx
          else filteredUnbalancedTxTx & over Tx.txBodyContentOuts (++ toList mNewTxOut)

  let txWithinputsAdded =
        if isZero neg
          then txWithOutputsAdded
          else txWithOutputsAdded & over Tx.txBodyContentIns (++ newTxIns)

  collateral <- traverse lookupValue (Tx.getTxBodyContentCollateralInputs txWithinputsAdded)
  let returnCollateral = Tx.getTxBodyContentReturnCollateral txWithinputsAdded

  if isZero (fold collateral)
    && null (C.collectTxBodyScriptWitnesses C.ShelleyBasedEraConway txWithinputsAdded) -- every script has a redeemer, no redeemers -> no scripts
    && null returnCollateral
    then -- Don't add collateral if there are no plutus scripts that can fail
    -- and there are no collateral inputs or outputs already
      pure (txWithinputsAdded, newInputs)
    else do
      let collAddr = maybe cChangeAddr (\(Tx.TxOut (C.TxOut aie _tov _tod _rs)) -> aie) returnCollateral
          collateralPercent = maybe 100 fromIntegral (C.protocolParamCollateralPercent (pProtocolParams params))
          collFees = (fees * collateralPercent + 99 {- make sure to round up -}) `div` 100
          collBalance = fold collateral <> lovelaceToValue (-collFees)

      ((negColl, newColInputs@(C.UTxO newColInputsMap)), (_, mNewTxOutColl)) <-
        calculateTxChanges params collAddr utxoProvider $ split collBalance

      case C.Api.protocolParamMaxCollateralInputs $ pProtocolParams params of
        Just maxInputs
          | length collateral + Map.size newColInputsMap > fromIntegral maxInputs ->
              errorReporter (Left (Phase1, MaxCollateralInputsExceeded))
        _ -> pure ()

      let newTxInsColl = Map.keys newColInputsMap

      let txWithCollateralInputs =
            if isZero negColl
              then txWithinputsAdded
              else txWithinputsAdded & over Tx.txBodyContentCollateralIns (++ newTxInsColl)

      let totalCollateral = toCardanoTotalCollateral (Just collFees)

      pure
        ( txWithCollateralInputs
            { C.txTotalCollateral = totalCollateral
            , C.txReturnCollateral = toCardanoReturnCollateral mNewTxOutColl
            }
        , newInputs <> newColInputs
        )

removeEmptyOutputsBuildTx :: C.TxBodyContent ctx C.ConwayEra -> C.TxBodyContent ctx C.ConwayEra
removeEmptyOutputsBuildTx bodyContent@C.TxBodyContent{C.txOuts} = bodyContent{C.txOuts = txOuts'}
  where
    txOuts' = filter (not . isEmpty' . Tx.TxOut) txOuts
    isEmpty' txOut =
      isZero (Tx.txOutValue txOut) && isNothing (Tx.txOutDatumHash txOut)

calculateTxChanges
  :: (Monad m)
  => Params
  -> C.AddressInEra C.ConwayEra
  -- ^ The address for the change output
  -> UtxoProvider m
  -- ^ The utxo provider
  -> (C.Value, C.Value)
  -- ^ The unbalanced tx's negative and positive balance.
  -> m ((C.Value, UtxoIndex), (C.Value, Maybe TxOut))
calculateTxChanges params addr utxoProvider (neg, pos) = do
  -- Calculate the change output with minimal ada
  let (newNeg, newPos, mExtraTxOut) =
        if isZero pos
          then (neg, pos, Nothing)
          else
            let txOut = C.TxOut addr (CardanoAPI.toCardanoTxOutValue pos) C.TxOutDatumNone C.Api.ReferenceScriptNone
                (missing, extraTxOut) = adjustTxOut (emulatorPParams params) (Tx.TxOut txOut)
                missingValue = lovelaceToValue (fold missing)
             in -- Add the missing ada to both sides to keep the balance.
                (neg <> missingValue, pos <> missingValue, Just extraTxOut)

  -- Calculate the extra inputs needed
  (spend, change) <-
    if isZero newNeg
      then pure (mempty, mempty)
      else utxoProvider newNeg

  if isZero change
    then do
      -- No change, so the new inputs and outputs have balanced the transaction
      pure ((newNeg, spend), (newPos, mExtraTxOut))
    else
      if null mExtraTxOut
        then -- We have change so we need an extra output, if we didn't have that yet,
        -- first make one with an estimated minimal amount of ada
        -- which then will calculate a more exact set of inputs

          calculateTxChanges
            params
            addr
            utxoProvider
            ( neg <> CardanoAPI.adaToCardanoValue minAdaTxOutEstimated
            , CardanoAPI.adaToCardanoValue minAdaTxOutEstimated
            )
        else -- Else recalculate with the change added to both sides
        -- Ideally this creates the same inputs and outputs and then the change will be zero
        -- But possibly the minimal Ada increases and then we also want to compute a new set of inputs
          calculateTxChanges params addr utxoProvider (newNeg <> change, newPos <> change)

data BalancingError
  = -- | Not enough extra inputs available to balance a transaction.
    InsufficientFunds {total :: C.Value, expected :: C.Value}
  | CardanoLedgerError CardanoLedgerError
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- Build a utxo provider from a set of unspent transaction outputs.
utxoProviderFromWalletOutputs
  :: UtxoIndex
  -- ^ The unspent transaction outputs.
  -> CardanoBuildTx
  -- ^ The transaction being balanced, to make sure that we don't reuse any inputs from it.
  -> UtxoProvider (Either BalancingError)
utxoProviderFromWalletOutputs (C.UTxO walletUtxos) unbalancedBodyContent value =
  let inputOutRefs = Tx.getTxBodyContentInputs $ CardanoAPI.getCardanoBuildTx unbalancedBodyContent
      -- filter out inputs from walletUtxos that are already in unbalancedBodyContent
      filteredUtxos = flip Map.filterWithKey walletUtxos $ \txOutRef _ ->
        txOutRef `notElem` inputOutRefs
      outRefsWithValue = (\p -> (p, Tx.cardanoTxOutValue (snd p))) <$> Map.toList filteredUtxos
   in first (C.UTxO . Map.fromList) <$> selectCoin outRefsWithValue value

{- | Given a set of @a@s with coin values, and a target value, select a number
of @a@ such that their total value is greater than or equal to the target.
-}
selectCoin
  :: (Eq a)
  => [(a, C.Value)]
  -- ^ Possible inputs to choose from
  -> C.Value
  -- ^ The target value
  -> Either BalancingError ([a], C.Value)
  -- ^ The chosen inputs and the change
selectCoin fnds vl =
  let
    total = foldMap snd fnds
    err = Left $ InsufficientFunds total vl
   in
    -- Values are in a partial order: what we want to check is that the
    -- total available funds are bigger than (or equal to) the required value.
    if not (total `valueGeq` vl)
      then err
      else -- Select inputs per asset class, sorting so we do Ada last.
      -- We want to do the non-Ada asset classes first, because utxo's often contain
      -- extra Ada because of fees or minAda constraints. So when we are done with the
      -- non-Ada asset classes we probably already have picked some Ada too.

        let (usedFinal, remainderFinal) = foldl' step ([], vl) (sortOn Down $ C.valueToList vl)
            step (used, remainder) (assetId, _) =
              let (used', remainder') = selectCoinSingle assetId (fnds \\ used) remainder
               in (used <> used', remainder')
         in pure (map fst usedFinal, C.negateValue remainderFinal)

selectCoinSingle
  :: C.AssetId
  -> [(a, C.Value)]
  -- ^ Possible inputs to choose from
  -> C.Value
  -- ^ The target value
  -> ([(a, C.Value)], C.Value)
  -- ^ The chosen inputs and the remainder
selectCoinSingle assetId fnds' vl =
  let
    pick v = C.selectAsset v assetId
    -- We only want the values that contain the given asset class,
    -- and want the single currency values first,
    -- so that we're picking inputs that contain *only* the given asset class when possible.
    -- That being equal we want the input with the largest amount of the given asset class,
    -- to reduce the amount of inputs required. (Particularly useful to prevent hitting MaxCollateralInputs)
    fnds =
      sortOn (length . C.valueToList . snd &&& Down . pick . snd) $ filter (\(_, v) -> pick v > 0) fnds'
    -- Given the funds of a wallet, we take just enough from
    -- the target value such that the asset class value of the remainder is <= 0.
    fundsWithRemainder = zip fnds (drop 1 $ scanl (\l r -> l <> C.negateValue r) vl $ fmap snd fnds)
    fundsToSpend = takeUntil (\(_, v) -> pick v <= 0) fundsWithRemainder
    remainder = maybe vl snd $ listToMaybe $ reverse fundsToSpend
   in
    (fst <$> fundsToSpend, remainder)

{- | Take elements from a list until the predicate is satisfied.
'takeUntil' @p@ includes the first element for wich @p@ is true
(unlike @takeWhile (not . p)@).
-}
takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ [] = []
takeUntil p (x : xs)
  | p x = [x]
  | otherwise = x : takeUntil p xs

fromLedgerUTxO
  :: UTxO EmulatorEra
  -> C.Api.UTxO C.Api.ConwayEra
fromLedgerUTxO (UTxO utxo) =
  C.Api.UTxO
    . Map.fromList
    . map (bimap C.Api.fromShelleyTxIn (C.Api.fromShelleyTxOut C.Api.ShelleyBasedEraConway))
    . Map.toList
    $ utxo

evaluateTransactionFee
  :: Params
  -> C.Api.TxBody C.Api.ConwayEra
  -> Word
  -> C.Api.Lovelace
evaluateTransactionFee params txbody keywitcount = C.evaluateTransactionFee C.shelleyBasedEra (emulatorPParams params) txbody keywitcount 0
