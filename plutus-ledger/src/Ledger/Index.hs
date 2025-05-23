{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | An index of unspent transaction outputs, and some functions for validating
-- transactions using the index.
module Ledger.Index
  ( -- * Types for transaction validation based on UTXO index
    UtxoIndex,
    insert,
    insertCollateral,
    insertBlock,
    initialise,
    singleton,
    lookup,
    lookupUTxO,
    getCollateral,
    ValidationError (..),
    _TxOutRefNotFound,
    _ScriptFailure,
    _CardanoLedgerValidationError,
    ValidationResult (..),
    _Success,
    _FailPhase1,
    _FailPhase2,
    cardanoTxFromValidationResult,
    toOnChain,
    getEvaluationLogs,
    ValidationSuccess,
    ValidationErrorInPhase,
    ValidationPhase (..),
    RedeemerReport,
    maxFee,
    adjustTxOut,
    minAdaTxOut,
    minAdaTxOutEstimated,
    minLovelaceTxOutEstimated,
    maxMinAdaTxOut,
    createGenesisTransaction,
    genesisTxIn,
    PV1.ExBudget (..),
    PV1.ExCPU (..),
    PV1.ExMemory (..),
    PV1.SatInt,
  )
where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C.Api
import Cardano.Ledger.Coin (Coin (Coin))
import Cardano.Ledger.Conway qualified as Conway
import Cardano.Ledger.Core (PParams, getMinCoinTxOut)
import Cardano.Ledger.Shelley.API qualified as C.Ledger
import Control.Lens (alaf, (&), (.~), (<&>))
import Data.Foldable (foldl')
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Monoid (Ap (..))
import Data.Set qualified as Set
import Ledger.Address (CardanoAddress)
import Ledger.Blockchain
import Ledger.Index.Internal
import Ledger.Orphans ()
import Ledger.Tx
  ( CardanoTx (..),
    TxOut (..),
    getCardanoTxCollateralInputs,
    getCardanoTxFee,
    getCardanoTxProducedOutputs,
    getCardanoTxProducedReturnCollateral,
    getCardanoTxSpentOutputs,
    getCardanoTxTotalCollateral,
    outValue,
    txOutValue,
  )
import Ledger.Tx.CardanoAPI (fromPlutusTxOut, toCardanoTxOutValue)
import Ledger.Tx.Internal qualified as Tx
import Ledger.Value.CardanoAPI (Value, lovelaceToValue)
import PlutusLedgerApi.V1 qualified as PV1
import PlutusTx.Lattice ((\/))
import Prelude hiding (lookup)

-- | Create an index of all UTxOs on the chain.
initialise :: Blockchain -> UtxoIndex
initialise = (`insertBlock` mempty) . concat

-- | Create an index with a single UTxO.
singleton :: C.TxIn -> C.TxOut C.CtxUTxO C.ConwayEra -> UtxoIndex
singleton txIn txOut = C.UTxO $ Map.singleton txIn txOut

-- | Update the index for the addition of a transaction.
insert :: CardanoTx -> UtxoIndex -> UtxoIndex
insert tx (C.UTxO unspent) =
  C.UTxO $
    (unspent `Map.withoutKeys` getCardanoTxSpentOutputs tx)
      `Map.union` (Tx.toCtxUTxOTxOut <$> getCardanoTxProducedOutputs tx)

-- | Update the index for the addition of only the collateral inputs of a failed transaction.
insertCollateral :: CardanoTx -> UtxoIndex -> UtxoIndex
insertCollateral tx (C.UTxO unspent) =
  C.UTxO $
    (unspent `Map.withoutKeys` Set.fromList (getCardanoTxCollateralInputs tx))
      `Map.union` (Tx.toCtxUTxOTxOut <$> getCardanoTxProducedReturnCollateral tx)

-- | Update the index for the addition of a block.
insertBlock :: Block -> UtxoIndex -> UtxoIndex
insertBlock blck i = foldl' (flip (eitherTx insertCollateral insert)) i blck

-- | Find an unspent transaction output by the 'TxIn' that spends it.
lookup :: C.TxIn -> UtxoIndex -> Maybe TxOut
lookup i index = case lookupUTxO i index of
  Just (C.TxOut aie tov tod rs) ->
    let tod' = case tod of
          C.TxOutDatumNone -> C.TxOutDatumNone
          C.TxOutDatumHash era scriptDataHash -> C.TxOutDatumHash era scriptDataHash
          C.TxOutDatumInline era scriptData -> C.TxOutDatumInline era scriptData
     in Just $ TxOut (C.TxOut aie tov tod' rs)
  Nothing -> Nothing

-- | Find an unspent transaction output (using the Ledger type) by the 'TxIn' that spends it.
lookupUTxO :: C.TxIn -> UtxoIndex -> Maybe (C.TxOut C.CtxUTxO C.ConwayEra)
lookupUTxO i index = Map.lookup i $ C.unUTxO index

getCollateral :: UtxoIndex -> CardanoTx -> C.Value
getCollateral idx tx = case getCardanoTxTotalCollateral tx of
  Just v -> lovelaceToValue v
  Nothing ->
    fromMaybe (lovelaceToValue $ getCardanoTxFee tx) $
      alaf Ap foldMap (fmap txOutValue . (`lookup` idx)) (getCardanoTxCollateralInputs tx)

-- | Adjust a single transaction output so it contains at least the minimum amount of Ada
-- and return the adjustment (if any) and the updated TxOut.
adjustTxOut :: PParams Conway.ConwayEra -> TxOut -> ([Coin], Tx.TxOut)
adjustTxOut params txOut = do
  -- Increasing the ada amount can also increase the size in bytes, so start with a rough estimated amount of ada
  let withMinAdaValue = toCardanoTxOutValue $ txOutValue txOut \/ lovelaceToValue (minAdaTxOut params txOut)
  let txOutEstimate = txOut & outValue .~ withMinAdaValue
      minAdaTxOutEstimated' = minAdaTxOut params txOutEstimate
      missingLovelace = minAdaTxOutEstimated' - C.selectLovelace (txOutValue txOut)
  if missingLovelace > 0
    then
      let adjustedLovelace = toCardanoTxOutValue $ txOutValue txOut <> lovelaceToValue missingLovelace
       in ([missingLovelace], txOut & outValue .~ adjustedLovelace)
    else ([], txOut)

-- | Exact computation of the mimimum Ada required for a given TxOut.
-- TODO: Should be moved to cardano-api-extended once created
minAdaTxOut :: PParams Conway.ConwayEra -> TxOut -> Coin
minAdaTxOut params txOut =
  let toLovelace = Coin . C.Ledger.unCoin
      initialValue = txOutValue txOut
      firstEstimate = toLovelace . getMinCoinTxOut params $ fromPlutusTxOut txOut -- if the estimate is above the initialValue, we run minAdaAgain, just to be sure that the
      -- new amount didn't change the TxOut size and requires more ada.
   in if firstEstimate > C.selectLovelace initialValue
        then
          minAdaTxOut params . flip (outValue .~) txOut $
            toCardanoTxOutValue $
              lovelaceToValue firstEstimate \/ initialValue
        else firstEstimate

{-# INLINEABLE minAdaTxOutEstimated #-}

-- | Provide a reasonable estimate of the mimimum of Ada required for a TxOut.
--
--  An exact estimate of the the mimimum of Ada in a TxOut is determined by two things:
--    - the `PParams`, more precisely its 'coinPerUTxOWord' parameter.
--    - the size of the 'TxOut'.
-- In many situations though, we need to determine a plausible value for the minimum of Ada needed for a TxOut
-- without knowing much of the 'TxOut'.
-- This function provides a value big enough to balance UTxOs without
-- a large inlined data (larger than a hash) nor a complex val with a lot of minted values.
-- It's superior to the lowest minimum needed for an UTxO, as the lowest value require no datum.
-- An estimate of the minimum required Ada for each tx output.
minAdaTxOutEstimated :: PV1.Lovelace
minAdaTxOutEstimated = PV1.Lovelace minTxOut

minLovelaceTxOutEstimated :: Coin
minLovelaceTxOutEstimated = Coin minTxOut

{-# INLINEABLE minTxOut #-}
minTxOut :: Integer
minTxOut = 2_000_000

{-# INLINEABLE maxMinAdaTxOut #-}
{-
maxMinAdaTxOut = maxTxOutSize * coinsPerUTxOWord
coinsPerUTxOWord = 34_482
maxTxOutSize = utxoEntrySizeWithoutVal + maxValSizeInWords + dataHashSize
utxoEntrySizeWithoutVal = 27
maxValSizeInWords = 500
dataHashSize = 10

These values are partly protocol parameters-based, but since this is used in on-chain code
we want a constant to reduce code size.
-}
maxMinAdaTxOut :: PV1.Lovelace
maxMinAdaTxOut = PV1.Lovelace 18_516_834

-- | TODO Should be calculated based on the maximum script size permitted on
-- the Cardano blockchain.
maxFee :: PV1.Lovelace
maxFee = PV1.Lovelace 1_000_000

-- | cardano-ledger validation rules require the presence of inputs and
-- we have to provide a stub TxIn for the genesis transaction.
genesisTxIn :: C.TxIn
genesisTxIn = C.TxIn "01f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53" (C.TxIx 40_214)

createGenesisTransaction :: Map.Map CardanoAddress Value -> CardanoTx
createGenesisTransaction vals =
  let txBodyContent =
        Tx.emptyTxBodyContent
          { C.txIns = [(genesisTxIn, C.BuildTxWith (C.KeyWitness C.KeyWitnessForSpending))],
            C.txOuts =
              Map.toList vals <&> \(changeAddr, v) ->
                C.TxOut changeAddr (toCardanoTxOutValue v) C.TxOutDatumNone C.Api.ReferenceScriptNone
          }
      txBody =
        either (error . ("createGenesisTransaction: Can't create TxBody: " <>) . show) id $
          C.createTransactionBody C.shelleyBasedEra txBodyContent
   in CardanoEmulatorEraTx $ C.Tx txBody []
