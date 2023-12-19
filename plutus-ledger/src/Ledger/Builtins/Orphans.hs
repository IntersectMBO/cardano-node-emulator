{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ledger.Builtins.Orphans where

import PlutusTx.Prelude qualified as PlutusTx

import Data.Aeson.Extras qualified as JSON

import Codec.Serialise (Serialise (decode, encode))
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import Data.Aeson qualified as JSON
import PlutusCore.Data
import PlutusTx qualified

instance ToJSON PlutusTx.BuiltinByteString where
  toJSON = JSON.String . JSON.encodeByteString . PlutusTx.fromBuiltin

instance FromJSON PlutusTx.BuiltinByteString where
  parseJSON v = PlutusTx.toBuiltin <$> JSON.decodeByteString v

instance ToJSON PlutusTx.BuiltinData where
  toJSON = toJSON . PlutusTx.builtinDataToData

instance FromJSON PlutusTx.BuiltinData where
  parseJSON = fmap PlutusTx.dataToBuiltinData . parseJSON

instance Serialise PlutusTx.BuiltinData where
  encode = encode . PlutusTx.builtinDataToData
  decode = PlutusTx.dataToBuiltinData <$> decode

deriving via (JSON.JSONViaSerialise Data) instance ToJSON Data
deriving via (JSON.JSONViaSerialise Data) instance FromJSON Data
