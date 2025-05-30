{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}

module Ledger.Tx.Internal
  ( module Ledger.Tx.Internal,
    Language (..),
    TxOut (..),
    TxOutRef (..),
    Versioned (..),
  )
where

import Cardano.Api (TxBodyContent (txValidityLowerBound))
import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.Binary qualified as C
import Cardano.Ledger.Alonzo.Genesis ()
import Codec.Serialise (Serialise, decode, encode)
import Control.Lens qualified as L
import Data.Aeson (FromJSON, ToJSON)
import Data.Map (Map)
import Data.Map qualified as Map
import GHC.Generics (Generic)
import Ledger.Address (CardanoAddress, cardanoPubKeyHash)
import Ledger.Crypto
import Ledger.DCert.Orphans ()
import Ledger.Tx.Orphans ()
import Ledger.Tx.Orphans.V2 ()
import Plutus.Script.Utils.Data (datumHash)
import Plutus.Script.Utils.Scripts
import PlutusLedgerApi.V1 (Credential, DCert, dataToBuiltinData)
import PlutusLedgerApi.V1.Scripts
import PlutusLedgerApi.V3.Tx (TxOutRef (..))
import PlutusTx (FromData (..), fromData)
import PlutusTx.Prelude qualified as PlutusTx
import Prettyprinter (Pretty (..), viaShow)

cardanoTxOutValue :: C.TxOut ctx era -> C.Value
cardanoTxOutValue (C.TxOut _aie tov _tod _rs) =
  C.txOutValueToValue tov

txOutValue :: TxOut -> C.Value
txOutValue = cardanoTxOutValue . getTxOut

outValue :: L.Lens TxOut TxOut C.Value (C.TxOutValue C.ConwayEra)
outValue =
  L.lens
    txOutValue
    (\(TxOut (C.TxOut aie _ tod rs)) tov -> TxOut (C.TxOut aie tov tod rs))

outValue' :: L.Lens' TxOut (C.TxOutValue C.ConwayEra)
outValue' =
  L.lens
    (\(TxOut (C.TxOut _aie tov _tod _rs)) -> tov)
    (\(TxOut (C.TxOut aie _ tod rs)) tov -> TxOut (C.TxOut aie tov tod rs))

-- | Stake withdrawal, if applicable the script should be included in txScripts.
data Withdrawal = Withdrawal
  { -- | staking credential
    withdrawalCredential :: Credential,
    -- | amount of withdrawal in Lovelace, must withdraw all eligible amount
    withdrawalAmount :: Integer,
    -- | redeemer for script credential
    withdrawalRedeemer :: Maybe Redeemer
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Serialise)

instance Pretty Withdrawal where
  pretty = viaShow

data Certificate = Certificate
  { certificateDcert :: DCert,
    -- | redeemer for script credential
    certificateRedeemer :: Maybe Redeemer
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Serialise)

instance Pretty Certificate where
  pretty = viaShow

newtype TxOut = TxOut {getTxOut :: C.TxOut C.CtxTx C.ConwayEra}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
  deriving newtype (Pretty)

instance C.ToCBOR TxOut where
  toCBOR = C.toCBOR . C.toShelleyTxOut C.ShelleyBasedEraConway . toCtxUTxOTxOut

instance C.FromCBOR TxOut where
  fromCBOR = TxOut . C.fromShelleyTxOut C.ShelleyBasedEraConway <$> C.fromCBOR

instance Serialise TxOut where
  encode = C.toCBOR
  decode = C.fromCBOR

toCtxUTxOTxOut :: TxOut -> C.TxOut C.CtxUTxO C.ConwayEra
toCtxUTxOTxOut = C.toCtxUTxOTxOut . getTxOut

type ScriptsMap = Map ScriptHash (Versioned Script)

type MintingWitnessesMap = Map MintingPolicyHash (Redeemer, Maybe (Versioned TxOutRef))

-- | Get a hash from the stored TxOutDatum (either directly or by hashing the inlined datum)
txOutDatumHash :: TxOut -> Maybe DatumHash
txOutDatumHash (TxOut (C.TxOut _aie _tov tod _rs)) =
  case tod of
    C.TxOutDatumNone ->
      Nothing
    C.TxOutDatumHash _era scriptDataHash ->
      Just $ DatumHash $ PlutusTx.toBuiltin (C.serialiseToRawBytes scriptDataHash)
    C.TxOutDatumInline _era scriptData ->
      Just $ datumHash $ Datum $ dataToBuiltinData $ C.toPlutusData $ C.getScriptData scriptData
    C.TxOutSupplementalDatum _era scriptData ->
      Just $ datumHash $ Datum $ dataToBuiltinData $ C.toPlutusData $ C.getScriptData scriptData

txOutDatum :: forall d. (FromData d) => TxOut -> Maybe d
txOutDatum (TxOut (C.TxOut _aie _tov tod _rs)) =
  case tod of
    C.TxOutDatumNone ->
      Nothing
    C.TxOutDatumHash _era _scriptDataHash ->
      Nothing
    C.TxOutDatumInline _era scriptData ->
      fromData @d $ C.toPlutusData $ C.getScriptData scriptData
    C.TxOutSupplementalDatum _era scriptData ->
      fromData @d $ C.toPlutusData $ C.getScriptData scriptData

cardanoTxOutDatumHash :: C.TxOutDatum C.CtxUTxO C.ConwayEra -> Maybe (C.Hash C.ScriptData)
cardanoTxOutDatumHash = \case
  C.TxOutDatumNone ->
    Nothing
  C.TxOutDatumHash _era scriptDataHash ->
    Just scriptDataHash
  C.TxOutDatumInline _era scriptData -> Just $ C.hashScriptDataBytes scriptData

txOutPubKey :: TxOut -> Maybe PubKeyHash
txOutPubKey (TxOut (C.TxOut aie _ _ _)) = cardanoPubKeyHash aie

txOutAddress :: TxOut -> CardanoAddress
txOutAddress (TxOut (C.TxOut aie _tov _tod _rs)) = aie

outAddress :: L.Lens' TxOut (C.AddressInEra C.ConwayEra)
outAddress =
  L.lens
    txOutAddress
    (\(TxOut (C.TxOut _ tov tod rs)) aie -> TxOut (C.TxOut aie tov tod rs))

outDatumHash :: L.Lens TxOut TxOut (Maybe DatumHash) (C.TxOutDatum C.CtxTx C.ConwayEra)
outDatumHash =
  L.lens
    txOutDatumHash
    (\(TxOut (C.TxOut aie tov _ rs)) tod -> TxOut (C.TxOut aie tov tod rs))

type ReferenceScript = C.ReferenceScript C.ConwayEra

txOutReferenceScript :: TxOut -> ReferenceScript
txOutReferenceScript (TxOut (C.TxOut _aie _tov _tod rs)) = rs

outReferenceScript :: L.Lens' TxOut ReferenceScript
outReferenceScript =
  L.lens
    txOutReferenceScript
    (\(TxOut (C.TxOut aie tov tod _)) rs -> TxOut (C.TxOut aie tov tod rs))

lookupScript :: ScriptsMap -> ScriptHash -> Maybe (Versioned Script)
lookupScript txScripts hash = Map.lookup hash txScripts

lookupValidator :: ScriptsMap -> ValidatorHash -> Maybe (Versioned Validator)
lookupValidator txScripts = (fmap . fmap) Validator . lookupScript txScripts . toScriptHash

lookupMintingPolicy :: ScriptsMap -> MintingPolicyHash -> Maybe (Versioned MintingPolicy)
lookupMintingPolicy txScripts = (fmap . fmap) MintingPolicy . lookupScript txScripts . toScriptHash

lookupStakeValidator :: ScriptsMap -> StakeValidatorHash -> Maybe (Versioned StakeValidator)
lookupStakeValidator txScripts = (fmap . fmap) StakeValidator . lookupScript txScripts . toScriptHash

emptyTxBodyContent :: C.TxBodyContent C.BuildTx C.ConwayEra
emptyTxBodyContent =
  C.TxBodyContent
    { txIns = [],
      txInsCollateral = C.TxInsCollateralNone,
      txMintValue = C.TxMintNone,
      txFee = C.TxFeeExplicit C.shelleyBasedEra 0,
      txOuts = [],
      txProtocolParams = C.BuildTxWith Nothing,
      txInsReference = C.TxInsReferenceNone,
      txTotalCollateral = C.TxTotalCollateralNone,
      txReturnCollateral = C.TxReturnCollateralNone,
      txValidityLowerBound = C.TxValidityNoLowerBound,
      txValidityUpperBound = C.TxValidityUpperBound C.shelleyBasedEra Nothing,
      txScriptValidity = C.TxScriptValidityNone,
      txExtraKeyWits = C.TxExtraKeyWitnessesNone,
      txMetadata = C.TxMetadataNone,
      txAuxScripts = C.TxAuxScriptsNone,
      txWithdrawals = C.TxWithdrawalsNone,
      txCertificates = C.TxCertificatesNone,
      txUpdateProposal = C.TxUpdateProposalNone,
      txProposalProcedures = Nothing,
      txVotingProcedures = Nothing,
      txCurrentTreasuryValue = Nothing,
      txTreasuryDonation = Nothing
    }
