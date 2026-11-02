{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ledger.Scripts.V3
  ( toCardanoScript,
  )
where

import Cardano.Api qualified as C
import Ledger.Scripts (ToCardanoAddress (toCardanoAddress))
import Plutus.Script.Utils.Address (ToAddress (toAddress))
import Plutus.Script.Utils.Scripts
  ( Language (PlutusV3),
    MintingPolicy,
    Script,
    StakeValidator,
    ToMintingPolicyHash (toMintingPolicyHash),
    ToScriptHash (toScriptHash),
    ToStakeValidatorHash (toStakeValidatorHash),
    ToValidatorHash (toValidatorHash),
    Validator,
    Versioned (Versioned),
  )
import Plutus.Script.Utils.Scripts qualified as PSU

instance ToScriptHash Script where
  toScriptHash = toScriptHash . (`Versioned` PlutusV3)

instance ToValidatorHash Validator where
  toValidatorHash = toValidatorHash . (`Versioned` PlutusV3)

instance ToMintingPolicyHash MintingPolicy where
  toMintingPolicyHash = toMintingPolicyHash . (`Versioned` PlutusV3)

instance ToStakeValidatorHash StakeValidator where
  toStakeValidatorHash = toStakeValidatorHash . (`Versioned` PlutusV3)

instance ToAddress Validator where
  toAddress = toAddress . (`Versioned` PlutusV3)

instance ToCardanoAddress Script where
  toCardanoAddress networkId = toCardanoAddress networkId . (`Versioned` PlutusV3)

instance ToCardanoAddress Validator where
  toCardanoAddress networkId = toCardanoAddress networkId . (`Versioned` PlutusV3)

instance ToCardanoAddress StakeValidator where
  toCardanoAddress networkId = toCardanoAddress networkId . (`Versioned` PlutusV3)

instance ToCardanoAddress MintingPolicy where
  toCardanoAddress networkId = toCardanoAddress networkId . (`Versioned` PlutusV3)

toCardanoScript :: Script -> C.Script C.PlutusScriptV3
toCardanoScript =
  C.PlutusScript C.PlutusScriptV3
    . C.PlutusScriptSerialised
    . PSU.unScript
