{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ledger.Crypto.Orphans where

import Codec.Serialise (Serialise)
import Control.Newtype.Generics (Newtype)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Hashable (Hashable)
import Ledger.Builtins.Orphans ()
import PlutusLedgerApi.V1.Crypto

deriving anyclass instance ToJSON PubKeyHash

deriving anyclass instance FromJSON PubKeyHash

deriving anyclass instance FromJSONKey PubKeyHash

deriving anyclass instance ToJSONKey PubKeyHash

deriving anyclass instance Newtype PubKeyHash

deriving newtype instance Serialise PubKeyHash

deriving newtype instance Hashable PubKeyHash
