{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ledger.Scripts.Orphans where

import Codec.Serialise (Serialise)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Hashable (Hashable)
import Ledger.Builtins.Orphans ()
import PlutusLedgerApi.V1.Scripts

deriving anyclass instance ToJSON DatumHash

deriving anyclass instance FromJSON DatumHash

deriving anyclass instance ToJSONKey DatumHash

deriving anyclass instance FromJSONKey DatumHash

deriving newtype instance Hashable DatumHash

deriving newtype instance Serialise DatumHash

deriving anyclass instance ToJSON RedeemerHash

deriving anyclass instance FromJSON RedeemerHash

deriving anyclass instance ToJSONKey RedeemerHash

deriving anyclass instance FromJSONKey RedeemerHash

deriving newtype instance Hashable RedeemerHash

deriving newtype instance Serialise RedeemerHash

deriving anyclass instance ToJSON ScriptHash

deriving anyclass instance FromJSON ScriptHash

deriving anyclass instance ToJSONKey ScriptHash

deriving anyclass instance FromJSONKey ScriptHash

deriving newtype instance Hashable ScriptHash

deriving newtype instance Serialise ScriptHash

deriving newtype instance ToJSON Context

deriving newtype instance FromJSON Context

deriving anyclass instance ToJSON Redeemer

deriving anyclass instance FromJSON Redeemer

deriving anyclass instance ToJSON Datum

deriving anyclass instance FromJSON Datum
