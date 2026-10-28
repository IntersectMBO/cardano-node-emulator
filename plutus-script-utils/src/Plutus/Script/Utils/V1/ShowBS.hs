{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}

-- | On-chain pretty-printing to 'BuiltinString' of the types reachable from the
-- Plutus V1 'ScriptContext'. The instances for the types shared with every other
-- version live in "Plutus.Script.Utils.ShowBS", which this module re-exports the
-- instances of.
module Plutus.Script.Utils.V1.ShowBS () where

import Plutus.Script.Utils.ShowBS
import PlutusLedgerApi.V1 qualified as V1
import PlutusTx.Prelude

instance ShowBS V1.Address where
  {-# INLINEABLE showBS #-}
  showBS (V1.Address cred mStCred) = application2 "Address" cred mStCred

instance ShowBS V1.TxId where
  {-# INLINEABLE showBS #-}
  showBS (V1.TxId x) = application1 "TxId" x

instance ShowBS V1.TxOutRef where
  {-# INLINEABLE showBS #-}
  showBS (V1.TxOutRef txid i) = application2 "TxOutRef" txid i

instance ShowBS V1.TxOut where
  {-# INLINEABLE showBS #-}
  showBS (V1.TxOut address value mDatumHash) = application3 "TxOut" address value mDatumHash

instance ShowBS V1.TxInInfo where
  {-# INLINEABLE showBS #-}
  showBS (V1.TxInInfo oref out) = application2 "TxInInfo" oref out

instance ShowBS V1.DCert where
  {-# INLINEABLE showBS #-}
  showBS (V1.DCertDelegRegKey stCred) = application1 "DCertDelegRegKey" stCred
  showBS (V1.DCertDelegDeRegKey stCred) = application1 "DCertDelegDeRegKey" stCred
  showBS (V1.DCertDelegDelegate stCred pkh) = application2 "DCertDelegDelegate" stCred pkh
  showBS (V1.DCertPoolRegister poolId poolVFR) = application2 "DCertPoolRegister" poolId poolVFR
  showBS (V1.DCertPoolRetire pkh epoch) = application2 "DCertPoolRetire" pkh epoch
  showBS V1.DCertGenesis = "DCertGenesis"
  showBS V1.DCertMir = "DCertMir"

instance ShowBS V1.ScriptPurpose where
  {-# INLINEABLE showBS #-}
  showBS (V1.Minting cs) = application1 "Minting" cs
  showBS (V1.Spending oref) = application1 "Spending" oref
  showBS (V1.Rewarding stCred) = application1 "Rewarding" stCred
  showBS (V1.Certifying dcert) = application1 "Certifying" dcert

instance ShowBS V1.TxInfo where
  {-# INLINEABLE showBS #-}
  showBS V1.TxInfo {..} =
    showBSParen
      $ "inputs:"
      <> showBS txInfoInputs
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
      <> "datums:"
      <> showBS txInfoData
      <> "transaction id:"
      <> showBS txInfoId

instance ShowBS V1.ScriptContext where
  {-# INLINEABLE showBS #-}
  showBS V1.ScriptContext {..} =
    showBSParen
      $ "Script context:"
      <> "Script Tx info:"
      <> showBS scriptContextTxInfo
      <> "Script purpose:"
      <> showBS scriptContextPurpose
