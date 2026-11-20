{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ledger.Scripts
  ( hashVersionedScript,
    ToCardanoScript (..),
  )
where

import Cardano.Api qualified as C.Api
import Ledger.Scripts.Orphans ()
import Plutus.Script.Utils.Scripts qualified as PSU

hashVersionedScript :: (PSU.ToScript a) => Versioned a -> C.Api.ScriptHash
hashVersionedScript (Versioned (PSU.toScript -> PSU.Script script) lang) = case lang of
  PlutusV1 -> C.Api.hashScript $ C.Api.PlutusScript C.Api.PlutusScriptV1 $ C.Api.PlutusScriptSerialised script
  PlutusV2 -> C.Api.hashScript $ C.Api.PlutusScript C.Api.PlutusScriptV2 $ C.Api.PlutusScriptSerialised script
  PlutusV3 -> C.Api.hashScript $ C.Api.PlutusScript C.Api.PlutusScriptV3 $ C.Api.PlutusScriptSerialised script

instance PSU.ToScriptHash C.Api.ScriptHash where
  toScriptHash = Api.ScriptHash . Api.toBuiltin . C.Api.serialiseToRawBytes

instance (PSU.ToScript a) => ToScriptHash (Versioned a) where
  toScriptHash = toScriptHash . hashVersionedScript

instance (PSU.ToScript a) => PSU.ToScriptHash (PSU.Versioned a) where
  toScriptHash = PSU.toScriptHash . hashVersionedScript

instance (PSU.ToScript a) => PSU.ToCredential (PSU.Versioned a) where
  toCredential = Api.ScriptCredential . PSU.toScriptHash

instance PSU.ToValidatorHash (PSU.Versioned PSU.Validator) where
  toValidatorHash = PSU.toValidatorHash . PSU.toScriptHash . fmap PSU.toScript

instance PSU.ToValidatorHash (PSU.Versioned PSU.Script) where
  toValidatorHash = PSU.toValidatorHash . PSU.toScriptHash

instance PSU.ToMintingPolicyHash (PSU.Versioned PSU.MintingPolicy) where
  toMintingPolicyHash = PSU.toMintingPolicyHash . PSU.toScriptHash . fmap PSU.toScript

instance PSU.ToMintingPolicyHash (PSU.Versioned PSU.Script) where
  toMintingPolicyHash = PSU.toMintingPolicyHash . PSU.toScriptHash

instance PSU.ToStakeValidatorHash (PSU.Versioned PSU.StakeValidator) where
  toStakeValidatorHash = PSU.toStakeValidatorHash . PSU.toScriptHash . fmap PSU.toScript

instance PSU.ToStakeValidatorHash (PSU.Versioned PSU.Script) where
  toStakeValidatorHash = PSU.toStakeValidatorHash . PSU.toScriptHash
