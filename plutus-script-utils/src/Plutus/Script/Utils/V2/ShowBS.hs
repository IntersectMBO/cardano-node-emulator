{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}

-- | On-chain pretty-printing to 'BuiltinString' of the types that are
-- introduced by the Plutus V2 'ScriptContext'. The types shared with V1 (and
-- with every version) are handled by "Plutus.Script.Utils.V1.ShowBS" and
-- "Plutus.Script.Utils.ShowBS", whose instances this module re-exports.
module Plutus.Script.Utils.V2.ShowBS () where

import Plutus.Script.Utils.ShowBS
import Plutus.Script.Utils.V1.ShowBS ()
import PlutusLedgerApi.V2 qualified as V2
import PlutusTx.Prelude

instance ShowBS V2.OutputDatum where
  {-# INLINEABLE showBS #-}
  showBS V2.NoOutputDatum = "NoOutputDatum"
  showBS (V2.OutputDatumHash h) = application1 "OutputDatumHash" h
  showBS (V2.OutputDatum d) = application1 "OutputDatum" d

instance ShowBS V2.TxOut where
  {-# INLINEABLE showBS #-}
  showBS (V2.TxOut address value datum mRefScriptHash) = application4 "TxOut" address value datum mRefScriptHash

instance ShowBS V2.TxInInfo where
  {-# INLINEABLE showBS #-}
  showBS (V2.TxInInfo oref out) = application2 "TxInInfo" oref out

instance ShowBS V2.TxInfo where
  {-# INLINEABLE showBS #-}
  showBS V2.TxInfo {..} =
    showBSParen
      $ "inputs:"
      <> showBS txInfoInputs
      <> "reference inputs:"
      <> showBS txInfoReferenceInputs
      <> "outputs:"
      <> showBS txInfoOutputs
      <> "fees:"
      <> showBS txInfoFee
      <> "minted value:"
      <> showBS txInfoMint
      <> "certificates:"
      <> showBS txInfoDCert
      <> "wdrl:"
      <> showBS txInfoWdrl
      <> "valid range:"
      <> showBS txInfoValidRange
      <> "signatories:"
      <> showBS txInfoSignatories
      <> "redeemers:"
      <> showBS txInfoRedeemers
      <> "datums:"
      <> showBS txInfoData
      <> "transaction id:"
      <> showBS txInfoId

instance ShowBS V2.ScriptContext where
  {-# INLINEABLE showBS #-}
  showBS V2.ScriptContext {..} =
    showBSParen
      $ "Script context:"
      <> "Script Tx info:"
      <> showBS scriptContextTxInfo
      <> "Script purpose:"
      <> showBS scriptContextPurpose
