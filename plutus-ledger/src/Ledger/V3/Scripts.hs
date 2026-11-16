{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ledger.V3.Scripts (toCardanoScript) where

import Cardano.Api qualified as C.Api
import Ledger.Address (ToCardanoAddress (toCardanoAddress))
import Ledger.Scripts (ToCardanoScript (toCardanoScript), ToCardanoScriptHash (toCardanoScriptHash))
import Plutus.Script.Utils.V3
import PlutusLedgerApi.Common (PlutusLedgerLanguage (PlutusV3))
import PlutusLedgerApi.V1.Address (Address (Address))

instance (ToScript a) => ToAddress (Versioned a) where
  toAddress = (`Address` Nothing) . toCredential

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

instance ToCardanoScript C.Api.PlutusScriptV3 Script where
  toCardanoScript = C.Api.PlutusScript C.Api.PlutusScriptV3 . C.Api.PlutusScriptSerialised . unScript

instance ToCardanoScriptHash Script where
  toCardanoScriptHash = C.Api.hashScript . toCardanoScript @C.Api.PlutusScriptV3
