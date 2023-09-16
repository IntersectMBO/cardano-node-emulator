{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ledger.Contexts.Orphans where

import PlutusLedgerApi.V1.Contexts (ScriptPurpose (..))

deriving stock instance Ord ScriptPurpose
