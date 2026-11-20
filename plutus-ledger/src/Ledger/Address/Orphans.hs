{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ledger.Address.Orphans where

import Cardano.Api qualified as C
import Codec.Serialise (Serialise)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import GHC.Generics (Generic)
import Ledger.Credential.Orphans ()
import Ledger.Scripts.Orphans ()
import PlutusLedgerApi.V1.Address

deriving anyclass instance ToJSON Address

deriving anyclass instance FromJSON Address

deriving anyclass instance Serialise Address

deriving instance Generic C.ShelleyWitnessSigningKey

instance (C.HasTextEnvelope (C.SigningKey a)) => ToJSON (C.SigningKey a) where
  toJSON = toJSON . C.serialiseToTextEnvelope Nothing

instance (C.HasTextEnvelope (C.SigningKey a)) => FromJSON (C.SigningKey a) where
  parseJSON v = do
    textEnvelope <- parseJSON v
    case C.deserialiseFromTextEnvelope @(C.SigningKey a) textEnvelope of
      Left err -> fail $ show err
      Right sk -> pure sk

deriving anyclass instance ToJSON C.ShelleyWitnessSigningKey

deriving anyclass instance FromJSON C.ShelleyWitnessSigningKey
