{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutus.Script.Utils.V4
  ( module X,
    toCardanoScript,
  )
where

import Cardano.Api qualified as C.Api
import Plutus.Script.Utils.Address as X
import Plutus.Script.Utils.Data as X
import Plutus.Script.Utils.Scripts as X
import Plutus.Script.Utils.V4.Contexts as X
import Plutus.Script.Utils.V4.Generators as X
import Plutus.Script.Utils.V4.Typed as X
import Plutus.Script.Utils.Value as X

instance ToValidatorHash Validator where
  {-# INLINEABLE toValidatorHash #-}
  toValidatorHash = toValidatorHash . (`Versioned` PlutusV4)

instance ToMintingPolicyHash MintingPolicy where
  {-# INLINEABLE toMintingPolicyHash #-}
  toMintingPolicyHash = toMintingPolicyHash . (`Versioned` PlutusV4)

instance ToStakeValidatorHash StakeValidator where
  {-# INLINEABLE toStakeValidatorHash #-}
  toStakeValidatorHash = toStakeValidatorHash . (`Versioned` PlutusV4)

instance ToScriptHash Script where
  {-# INLINEABLE toScriptHash #-}
  toScriptHash = toScriptHash . (`Versioned` PlutusV4)

instance ToAddress Validator where
  {-# INLINEABLE toAddress #-}
  toAddress = toAddress . (`Versioned` PlutusV4)

instance ToCardanoAddress Script where
  toCardanoAddress networkId = toCardanoAddress networkId . (`Versioned` PlutusV4)

instance ToCardanoAddress Validator where
  toCardanoAddress networkId = toCardanoAddress networkId . (`Versioned` PlutusV4)

instance ToCardanoAddress StakeValidator where
  toCardanoAddress networkId = toCardanoAddress networkId . (`Versioned` PlutusV4)

instance ToCardanoAddress MintingPolicy where
  toCardanoAddress networkId = toCardanoAddress networkId . (`Versioned` PlutusV4)

toCardanoScript :: Script -> C.Api.Script C.Api.PlutusScriptV4
toCardanoScript =
  C.Api.PlutusScript C.Api.PlutusScriptV4
    . C.Api.PlutusScriptSerialised
    . unScript
