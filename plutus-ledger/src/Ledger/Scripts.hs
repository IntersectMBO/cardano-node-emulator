{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ledger.Scripts
  ( module Export,
    ToCardanoAddress (..),
    hashVersionedScript,
    unitDatum,
    unitRedeemer,
    examplePlutusScriptAlwaysSucceeds,
    examplePlutusScriptAlwaysFails,
    examplePlutusScriptAlwaysSucceedsHash,
    examplePlutusScriptAlwaysFailsHash,
    WitCtx (..),
  )
where

import Cardano.Api
  ( PlutusScriptVersion (PlutusScriptV1),
    Script (PlutusScript),
    WitCtx (..),
    examplePlutusScriptAlwaysFails,
    examplePlutusScriptAlwaysSucceeds,
    hashScript,
    serialiseToRawBytes,
  )
import Cardano.Api qualified as C
import Ledger.Scripts.Orphans ()
import Plutus.Script.Utils.Address (ToAddress (toAddress), ToCredential (toCredential))
import Plutus.Script.Utils.Scripts as Export
import Plutus.Script.Utils.Scripts qualified as PSU
import PlutusLedgerApi.V1 qualified as PV1
import PlutusLedgerApi.V1.Scripts as Export
import PlutusTx (toBuiltinData)
import PlutusTx.Builtins (BuiltinByteString, toBuiltin)
import PlutusTx.Builtins qualified as Builtins

hashVersionedScript :: (PSU.ToScript a) => Versioned a -> C.ScriptHash
hashVersionedScript (Versioned (PSU.toScript -> PSU.Script script) lang) = case lang of
  PlutusV1 -> C.hashScript $ C.PlutusScript C.PlutusScriptV1 $ C.PlutusScriptSerialised script
  PlutusV2 -> C.hashScript $ C.PlutusScript C.PlutusScriptV2 $ C.PlutusScriptSerialised script
  PlutusV3 -> C.hashScript $ C.PlutusScript C.PlutusScriptV3 $ C.PlutusScriptSerialised script

instance ToScriptHash C.ScriptHash where
  toScriptHash = PV1.ScriptHash . Builtins.toBuiltin . C.serialiseToRawBytes

instance (PSU.ToScript a) => ToScriptHash (Versioned a) where
  toScriptHash = toScriptHash . hashVersionedScript

instance (PSU.ToScript a) => ToCredential (Versioned a) where
  toCredential = PV1.ScriptCredential . toScriptHash

instance (PSU.ToScript a) => ToAddress (Versioned a) where
  toAddress = (`PV1.Address` Nothing) . toCredential

class ToCardanoAddress a where
  toCardanoAddress :: C.NetworkId -> a -> C.AddressInEra C.ConwayEra

instance (PSU.ToScript a) => ToCardanoAddress (Versioned a) where
  toCardanoAddress networkId =
    flip (C.makeShelleyAddressInEra C.shelleyBasedEra networkId) C.NoStakeAddress
      . C.PaymentCredentialByScript
      . hashVersionedScript

instance ToValidatorHash (Versioned PSU.Validator) where
  toValidatorHash = toValidatorHash . toScriptHash . fmap PSU.toScript

instance ToValidatorHash (Versioned PSU.Script) where
  toValidatorHash = toValidatorHash . toScriptHash

instance ToMintingPolicyHash (Versioned PSU.MintingPolicy) where
  toMintingPolicyHash = toMintingPolicyHash . toScriptHash . fmap PSU.toScript

instance ToMintingPolicyHash (Versioned PSU.Script) where
  toMintingPolicyHash = toMintingPolicyHash . toScriptHash

instance ToStakeValidatorHash (Versioned PSU.StakeValidator) where
  toStakeValidatorHash = toStakeValidatorHash . toScriptHash . fmap PSU.toScript

instance ToStakeValidatorHash (Versioned PSU.Script) where
  toStakeValidatorHash = toStakeValidatorHash . toScriptHash

examplePlutusScriptAlwaysSucceedsHash :: WitCtx ctx -> BuiltinByteString
examplePlutusScriptAlwaysSucceedsHash =
  toBuiltin
    . serialiseToRawBytes
    . hashScript
    . PlutusScript PlutusScriptV1
    . examplePlutusScriptAlwaysSucceeds

examplePlutusScriptAlwaysFailsHash :: WitCtx ctx -> BuiltinByteString
examplePlutusScriptAlwaysFailsHash =
  toBuiltin
    . serialiseToRawBytes
    . hashScript
    . PlutusScript PlutusScriptV1
    . examplePlutusScriptAlwaysFails

-- | @()@ as a datum.
unitDatum :: Datum
unitDatum = Datum $ toBuiltinData ()

-- | @()@ as a redeemer.
unitRedeemer :: Redeemer
unitRedeemer = Redeemer $ toBuiltinData ()
