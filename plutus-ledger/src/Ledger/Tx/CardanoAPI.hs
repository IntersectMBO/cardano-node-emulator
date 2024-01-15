{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

{- |

Interface to the transaction types from 'cardano-api'
-}
module Ledger.Tx.CardanoAPI (
  module Ledger.Tx.CardanoAPI.Internal,
  CardanoBuildTx (..),
  CardanoTx (..),
  fromCardanoTxInsCollateral,
  fromCardanoTotalCollateral,
  fromCardanoReturnCollateral,
  toCardanoTotalCollateral,
  toCardanoReturnCollateral,
  toCardanoDatumWitness,
  toCardanoTxInReferenceWitnessHeader,
  toCardanoTxInScriptWitnessHeader,
  toCardanoMintWitness,
  ToCardanoError (..),
  FromCardanoError (..),
  getRequiredSigners,

  -- * Conversion from Plutus types
  fromPlutusIndex,
  fromPlutusTxOut,
  fromPlutusTxOutRef,
) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.Ledger.Alonzo.Tx (AlonzoTx (..))
import Cardano.Ledger.BaseTypes (mkTxIxPartial)
import Cardano.Ledger.Conway qualified as Conway
import Cardano.Ledger.Conway.TxBody (ConwayTxBody (ConwayTxBody, ctbReqSignerHashes))
import Cardano.Ledger.Core qualified as Ledger
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Shelley.API qualified as C.Ledger
import Ledger.Address qualified as P
import Ledger.Index.Internal qualified as P
import Ledger.Scripts qualified as P
import Ledger.Tx.CardanoAPI.Internal
import Ledger.Tx.Internal qualified as P
import Plutus.Script.Utils.Scripts qualified as PV1
import PlutusLedgerApi.V1 qualified as PV1

toCardanoMintWitness
  :: PV1.Redeemer
  -> Maybe (P.Versioned PV1.TxOutRef)
  -> Maybe (P.Versioned PV1.MintingPolicy)
  -> Either ToCardanoError (C.ScriptWitness C.WitCtxMint C.ConwayEra)
toCardanoMintWitness _ Nothing Nothing = Left MissingMintingPolicy
toCardanoMintWitness redeemer (Just ref) _ =
  toCardanoScriptWitness C.NoScriptDatumForMint redeemer (Right ref)
toCardanoMintWitness redeemer _ (Just script) =
  toCardanoScriptWitness C.NoScriptDatumForMint redeemer (Left (fmap P.getMintingPolicy script))

toCardanoScriptWitness
  :: (PV1.ToData a)
  => C.ScriptDatum witctx
  -> a
  -> Either (P.Versioned PV1.Script) (P.Versioned PV1.TxOutRef)
  -> Either ToCardanoError (C.ScriptWitness witctx C.ConwayEra)
toCardanoScriptWitness datum redeemer scriptOrRef =
  ( case scriptOrRef of
      Left script -> pure $ toCardanoTxInScriptWitnessHeader script
      Right ref -> toCardanoTxInReferenceWitnessHeader ref
  )
    <*> pure datum
    <*> pure (C.unsafeHashableScriptData $ C.fromPlutusData $ PV1.toData redeemer)
    <*> pure zeroExecutionUnits

fromCardanoTxInsCollateral :: C.TxInsCollateral era -> [C.TxIn]
fromCardanoTxInsCollateral C.TxInsCollateralNone = []
fromCardanoTxInsCollateral (C.TxInsCollateral _ txIns) = txIns

toCardanoDatumWitness :: Maybe PV1.Datum -> C.ScriptDatum C.WitCtxTxIn
toCardanoDatumWitness = maybe C.InlineScriptDatum (C.ScriptDatumForTxIn . toCardanoScriptData . PV1.getDatum)

type WitnessHeader witctx =
  C.ScriptDatum witctx -> C.ScriptRedeemer -> C.ExecutionUnits -> C.ScriptWitness witctx C.ConwayEra

toCardanoTxInReferenceWitnessHeader
  :: P.Versioned PV1.TxOutRef -> Either ToCardanoError (WitnessHeader witctx)
toCardanoTxInReferenceWitnessHeader (P.Versioned ref lang) = do
  txIn <- toCardanoTxIn ref
  pure $ case lang of
    P.PlutusV1 ->
      C.PlutusScriptWitness C.PlutusScriptV1InConway C.PlutusScriptV1 $
        C.PReferenceScript txIn Nothing
    P.PlutusV2 ->
      C.PlutusScriptWitness C.PlutusScriptV2InConway C.PlutusScriptV2 $
        C.PReferenceScript txIn Nothing
    P.PlutusV3 -> error "toCardanoTxInReferenceWitnessHeader: Plutus V3 not supported in Conway era"

toCardanoTxInScriptWitnessHeader :: P.Versioned PV1.Script -> WitnessHeader witctx
toCardanoTxInScriptWitnessHeader script =
  case toCardanoScriptInEra script of
    C.ScriptInEra _ (C.SimpleScript _) -> error "toCardanoTxInScriptWitnessHeader: impossible simple script"
    C.ScriptInEra era (C.PlutusScript v s) ->
      C.PlutusScriptWitness era v (C.PScript s)

fromCardanoTotalCollateral :: C.TxTotalCollateral C.ConwayEra -> Maybe C.Lovelace
fromCardanoTotalCollateral C.TxTotalCollateralNone = Nothing
fromCardanoTotalCollateral (C.TxTotalCollateral _ lv) = Just lv

toCardanoTotalCollateral :: Maybe C.Lovelace -> C.TxTotalCollateral C.ConwayEra
toCardanoTotalCollateral =
  maybe
    C.TxTotalCollateralNone
    (C.TxTotalCollateral C.BabbageEraOnwardsConway)

fromCardanoReturnCollateral :: C.TxReturnCollateral C.CtxTx C.ConwayEra -> Maybe P.TxOut
fromCardanoReturnCollateral C.TxReturnCollateralNone = Nothing
fromCardanoReturnCollateral (C.TxReturnCollateral _ txOut) = Just $ P.TxOut txOut

toCardanoReturnCollateral :: Maybe P.TxOut -> C.TxReturnCollateral C.CtxTx C.ConwayEra
toCardanoReturnCollateral =
  maybe
    C.TxReturnCollateralNone
    (C.TxReturnCollateral C.BabbageEraOnwardsConway . P.getTxOut)

getRequiredSigners :: C.Tx C.ConwayEra -> [P.PaymentPubKeyHash]
getRequiredSigners (C.ShelleyTx _ (AlonzoTx ConwayTxBody{ctbReqSignerHashes = rsq} _ _ _)) =
  foldMap
    (pure . P.PaymentPubKeyHash . P.toPlutusPubKeyHash . C.PaymentKeyHash . C.Ledger.coerceKeyRole)
    rsq

fromPlutusIndex :: P.UtxoIndex -> C.Ledger.UTxO (Conway.ConwayEra StandardCrypto)
fromPlutusIndex = C.toLedgerUTxO C.ShelleyBasedEraConway

fromPlutusTxOutRef :: P.TxOutRef -> Either ToCardanoError (C.Ledger.TxIn StandardCrypto)
fromPlutusTxOutRef (P.TxOutRef txId i) = C.Ledger.TxIn <$> fromPlutusTxId txId <*> pure (mkTxIxPartial i)

fromPlutusTxId :: PV1.TxId -> Either ToCardanoError (C.Ledger.TxId StandardCrypto)
fromPlutusTxId = fmap C.toShelleyTxId . toCardanoTxId

fromPlutusTxOut :: P.TxOut -> Ledger.TxOut (Conway.ConwayEra StandardCrypto)
fromPlutusTxOut = C.toShelleyTxOut C.ShelleyBasedEraConway . P.toCtxUTxOTxOut
