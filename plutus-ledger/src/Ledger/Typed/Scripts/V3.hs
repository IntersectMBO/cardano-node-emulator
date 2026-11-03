{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ledger.Typed.Scripts.V3 () where

import Ledger.Scripts ()
import Plutus.Script.Utils.Address (ToAddress (toAddress), ToCredential (toCredential))
import Plutus.Script.Utils.Scripts
  ( Script,
    ToMintingPolicyHash (toMintingPolicyHash),
    ToScriptHash (toScriptHash),
    ToStakeValidatorHash (toStakeValidatorHash),
    ToValidatorHash (toValidatorHash),
    ToVersioned (toVersioned),
  )
import Plutus.Script.Utils.V3.Typed (MultiPurposeScript)
import PlutusLedgerApi.V3 (Address (Address), Credential (ScriptCredential))

instance Show MultiPurposeScript where
  show = ("Multi-purpose script #" <>) . show . toScriptHash

instance ToScriptHash MultiPurposeScript where
  toScriptHash = toScriptHash . toVersioned @Script

instance ToCredential MultiPurposeScript where
  toCredential = ScriptCredential . toScriptHash

instance ToAddress MultiPurposeScript where
  toAddress = (`Address` Nothing) . toCredential

instance ToValidatorHash MultiPurposeScript where
  toValidatorHash = toValidatorHash . toScriptHash

instance ToMintingPolicyHash MultiPurposeScript where
  toMintingPolicyHash = toMintingPolicyHash . toScriptHash

instance ToStakeValidatorHash MultiPurposeScript where
  toStakeValidatorHash = toStakeValidatorHash . toScriptHash
