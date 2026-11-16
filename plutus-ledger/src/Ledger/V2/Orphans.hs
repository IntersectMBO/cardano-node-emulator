{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ledger.V2.Orphans where

import Codec.Serialise (Serialise)
import Data.Aeson (FromJSON, ToJSON)
import Ledger.Address.Orphans ()
import Ledger.Builtins.Orphans ()
import Ledger.Credential.Orphans ()
import Ledger.Scripts.Orphans ()
import Ledger.V1.Orphans ()
import Ledger.Value.Orphans ()
import PlutusLedgerApi.V2

deriving anyclass instance ToJSON OutputDatum

deriving anyclass instance FromJSON OutputDatum

deriving anyclass instance Serialise OutputDatum

deriving anyclass instance ToJSON TxOut

deriving anyclass instance FromJSON TxOut

deriving anyclass instance Serialise TxOut
