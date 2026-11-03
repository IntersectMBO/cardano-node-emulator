{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ledger.Typed.Scripts.Orphans where

import Data.Aeson (FromJSON, ToJSON)
import Ledger.Tx.Orphans ()
import Ledger.Typed.Scripts.V1 qualified as V1

deriving anyclass instance ToJSON V1.ConnectionError

deriving anyclass instance FromJSON V1.ConnectionError
