{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ledger.Tx.Orphans.V3 where

import Codec.Serialise (Serialise)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Ledger.Builtins.Orphans ()
import PlutusLedgerApi.V3 (TxId, TxOutRef)

deriving anyclass instance ToJSON TxOutRef

deriving anyclass instance FromJSON TxOutRef

deriving anyclass instance ToJSONKey TxOutRef

deriving anyclass instance FromJSONKey TxOutRef

deriving anyclass instance Serialise TxOutRef

deriving anyclass instance ToJSON TxId

deriving anyclass instance FromJSON TxId

deriving anyclass instance ToJSONKey TxId

deriving anyclass instance FromJSONKey TxId

deriving anyclass instance Serialise TxId
