{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ledger.V1.Scripts (toCardanoScript) where

import Cardano.Api qualified as C.Api
import Ledger.Address (ToCardanoAddress (toCardanoAddress))
import Ledger.Scripts (ToCardanoScript (toCardanoScript), ToCardanoScriptHash (toCardanoScriptHash))
import Plutus.Script.Utils.V1
import PlutusLedgerApi.Common (PlutusLedgerLanguage (PlutusV1))
import PlutusLedgerApi.V1.Address (Address (Address))

instance (ToScript a) => ToAddress (Versioned a) where
  toAddress = (`Address` Nothing) . toCredential

instance ToScriptHash Script where
  toScriptHash = toScriptHash . (`Versioned` PlutusV1)

instance ToValidatorHash Validator where
  toValidatorHash = toValidatorHash . (`Versioned` PlutusV1)

instance ToMintingPolicyHash MintingPolicy where
  toMintingPolicyHash = toMintingPolicyHash . (`Versioned` PlutusV1)

instance ToStakeValidatorHash StakeValidator where
  toStakeValidatorHash = toStakeValidatorHash . (`Versioned` PlutusV1)

instance ToAddress Validator where
  toAddress = toAddress . (`Versioned` PlutusV1)

instance ToCardanoAddress Script where
  toCardanoAddress networkId = toCardanoAddress networkId . (`Versioned` PlutusV1)

instance ToCardanoAddress Validator where
  toCardanoAddress networkId = toCardanoAddress networkId . (`Versioned` PlutusV1)

instance ToCardanoAddress StakeValidator where
  toCardanoAddress networkId = toCardanoAddress networkId . (`Versioned` PlutusV1)

instance ToCardanoAddress MintingPolicy where
  toCardanoAddress networkId = toCardanoAddress networkId . (`Versioned` PlutusV1)

instance ToCardanoScript C.Api.PlutusScriptV1 Script where
  toCardanoScript = C.Api.PlutusScript C.Api.PlutusScriptV1 . C.Api.PlutusScriptSerialised . unScript

instance ToCardanoScriptHash Script where
  toCardanoScriptHash = C.Api.hashScript . toCardanoScript @C.Api.PlutusScriptV1
