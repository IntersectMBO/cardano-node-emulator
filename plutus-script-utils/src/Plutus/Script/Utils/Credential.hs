{-# LANGUAGE NoImplicitPrelude #-}

module Plutus.Script.Utils.Credential
  ( ToCredential (..),
    ToPubKeyHash (..),
  )
where

import PlutusLedgerApi.V1 qualified as Api
import PlutusTx.Prelude (Either, either, id)

class ToCredential a where
  toCredential :: a -> Api.Credential

instance ToCredential Api.Credential where
  toCredential = id

instance ToCredential Api.ScriptHash where
  toCredential = Api.ScriptCredential

instance ToCredential Api.PubKeyHash where
  toCredential = Api.PubKeyCredential

instance ToCredential Api.Address where
  toCredential = Api.addressCredential

instance (ToCredential a, ToCredential b) => ToCredential (Either a b) where
  toCredential = either toCredential toCredential

class ToPubKeyHash a where
  toPubKeyHash :: a -> Api.PubKeyHash

instance ToPubKeyHash Api.PubKeyHash where
  toPubKeyHash = id
