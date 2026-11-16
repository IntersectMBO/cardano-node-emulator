{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ledger.V2.Scripts (toCardanoScript) where

import Cardano.Api qualified as C.Api
import Ledger.Address (ToCardanoAddress (toCardanoAddress))
import Ledger.Scripts (ToCardanoScript (toCardanoScript), ToCardanoScriptHash (toCardanoScriptHash))
import Plutus.Script.Utils.V2
import PlutusLedgerApi.Common (PlutusLedgerLanguage (PlutusV2))
import PlutusLedgerApi.V1.Address (Address (Address))

instance (ToScript a) => ToAddress (Versioned a) where
  toAddress = (`Address` Nothing) . toCredential

instance ToScriptHash Script where
  toScriptHash = toScriptHash . (`Versioned` PlutusV2)

instance ToValidatorHash Validator where
  toValidatorHash = toValidatorHash . (`Versioned` PlutusV2)

instance ToMintingPolicyHash MintingPolicy where
  toMintingPolicyHash = toMintingPolicyHash . (`Versioned` PlutusV2)

instance ToStakeValidatorHash StakeValidator where
  toStakeValidatorHash = toStakeValidatorHash . (`Versioned` PlutusV2)

instance ToAddress Validator where
  toAddress = toAddress . (`Versioned` PlutusV2)

instance ToCardanoAddress Script where
  toCardanoAddress networkId = toCardanoAddress networkId . (`Versioned` PlutusV2)

instance ToCardanoAddress Validator where
  toCardanoAddress networkId = toCardanoAddress networkId . (`Versioned` PlutusV2)

instance ToCardanoAddress StakeValidator where
  toCardanoAddress networkId = toCardanoAddress networkId . (`Versioned` PlutusV2)

instance ToCardanoAddress MintingPolicy where
  toCardanoAddress networkId = toCardanoAddress networkId . (`Versioned` PlutusV2)

instance ToCardanoScript C.Api.PlutusScriptV2 Script where
  toCardanoScript = C.Api.PlutusScript C.Api.PlutusScriptV2 . C.Api.PlutusScriptSerialised . unScript

instance ToCardanoScriptHash Script where
  toCardanoScriptHash = C.Api.hashScript . toCardanoScript @C.Api.PlutusScriptV2
