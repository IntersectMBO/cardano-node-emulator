{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ledger.Scripts.V2
  ( toCardanoScript,
    alwaysSucceedValidatorHash,
    alwaysFailValidatorHash,
    alwaysSucceedPolicyHash,
    alwaysFailPolicyHash,
    alwaysSucceedCurrencySymbol,
    alwaysSucceedTokenValue,
    alwaysFailCurrencySymbol,
    alwaysFailTokenValue,
  )
where

import Cardano.Api qualified as C
import Ledger.Scripts (ToCardanoAddress (toCardanoAddress))
import Plutus.Script.Utils.Address (ToAddress (toAddress))
import Plutus.Script.Utils.Scripts
  ( Language (PlutusV2),
    MintingPolicy,
    MintingPolicyHash,
    Script,
    StakeValidator,
    ToMintingPolicyHash (toMintingPolicyHash),
    ToScriptHash (toScriptHash),
    ToStakeValidatorHash (toStakeValidatorHash),
    ToValidatorHash (toValidatorHash),
    Validator,
    ValidatorHash,
    Versioned (Versioned),
    toCurrencySymbol,
  )
import Plutus.Script.Utils.Scripts qualified as PSU
import Plutus.Script.Utils.V2.Generators qualified as G
import PlutusLedgerApi.V2 (CurrencySymbol, TokenName, Value, singleton)

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

toCardanoScript :: Script -> C.Script C.PlutusScriptV2
toCardanoScript =
  C.PlutusScript C.PlutusScriptV2
    . C.PlutusScriptSerialised
    . PSU.unScript

alwaysSucceedValidatorHash :: ValidatorHash
alwaysSucceedValidatorHash = toValidatorHash G.alwaysSucceedValidatorVersioned

alwaysFailValidatorHash :: ValidatorHash
alwaysFailValidatorHash = toValidatorHash G.alwaysFailValidatorVersioned

alwaysSucceedPolicyHash :: MintingPolicyHash
alwaysSucceedPolicyHash = toMintingPolicyHash G.alwaysSucceedPolicyVersioned

alwaysFailPolicyHash :: MintingPolicyHash
alwaysFailPolicyHash = toMintingPolicyHash G.alwaysFailPolicyVersioned

alwaysSucceedCurrencySymbol :: CurrencySymbol
alwaysSucceedCurrencySymbol = toCurrencySymbol G.alwaysSucceedPolicyVersioned

alwaysSucceedTokenValue :: TokenName -> Integer -> Value
alwaysSucceedTokenValue = singleton alwaysSucceedCurrencySymbol

alwaysFailCurrencySymbol :: CurrencySymbol
alwaysFailCurrencySymbol = toCurrencySymbol G.alwaysFailPolicyVersioned

alwaysFailTokenValue :: TokenName -> Integer -> Value
alwaysFailTokenValue = singleton alwaysFailCurrencySymbol
