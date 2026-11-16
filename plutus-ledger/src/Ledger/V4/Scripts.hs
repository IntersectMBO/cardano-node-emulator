{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ledger.V4.Scripts (toCardanoScript) where

import Cardano.Api qualified as C.Api
import Ledger.Address (ToCardanoAddress (toCardanoAddress))
import Ledger.Scripts (ToCardanoScript (toCardanoScript), ToCardanoScriptHash (toCardanoScriptHash))
import Plutus.Script.Utils.V4
import PlutusLedgerApi.Common (PlutusLedgerLanguage (PlutusV4))
import PlutusLedgerApi.V4.Address (Address (Address))

instance (ToScript a) => ToAddress (Versioned a) where
  toAddress = (`Address` Nothing) . toCredential

instance ToScriptHash Script where
  toScriptHash = toScriptHash . (`Versioned` PlutusV4)

instance ToValidatorHash Validator where
  toValidatorHash = toValidatorHash . (`Versioned` PlutusV4)

instance ToMintingPolicyHash MintingPolicy where
  toMintingPolicyHash = toMintingPolicyHash . (`Versioned` PlutusV4)

instance ToStakeValidatorHash StakeValidator where
  toStakeValidatorHash = toStakeValidatorHash . (`Versioned` PlutusV4)

instance ToAddress Validator where
  toAddress = toAddress . (`Versioned` PlutusV4)

instance ToCardanoAddress Script where
  toCardanoAddress networkId = toCardanoAddress networkId . (`Versioned` PlutusV4)

instance ToCardanoAddress Validator where
  toCardanoAddress networkId = toCardanoAddress networkId . (`Versioned` PlutusV4)

instance ToCardanoAddress StakeValidator where
  toCardanoAddress networkId = toCardanoAddress networkId . (`Versioned` PlutusV4)

instance ToCardanoAddress MintingPolicy where
  toCardanoAddress networkId = toCardanoAddress networkId . (`Versioned` PlutusV4)

instance ToCardanoScript C.Api.PlutusScriptV4 Script where
  toCardanoScript = C.Api.PlutusScript C.Api.PlutusScriptV4 . C.Api.PlutusScriptSerialised . unScript

instance ToCardanoScriptHash Script where
  toCardanoScriptHash = C.Api.hashScript . toCardanoScript @C.Api.PlutusScriptV4
