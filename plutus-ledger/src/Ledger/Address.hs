{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ledger.Address
  ( LedgerAddress,
    PaymentPrivateKey (..),
    PaymentPubKey (..),
    PaymentPubKeyHash (..),
    StakePrivateKey (..),
    StakePubKey (..),
    StakePubKeyHash (..),
    ToWitness (..),
    ToCardanoAddress (..),
    fromCardanoAddress,
    fromKeyHash,
    fromScriptHash,
    fromByronAddress,
    fromCardanoStakeCredential,
    paymentPubKeyHash,
    pubKeyHashAddress,
    pubKeyAddress,
    scriptValidatorHashAddress,
    stakePubKeyHashCredential,
    stakeValidatorHashCredential,
    xprvToPaymentPubKey,
    xprvToPaymentPubKeyHash,
    xprvToAccountId,
    xprvToStakePubKey,
    xprvToStakePubKeyHash,
  )
where

import Cardano.Api qualified as C
import Cardano.Api.Byron qualified as Byron
import Cardano.Api.Ledger qualified as C
import Cardano.Chain.Common (addrToBase58)
import Cardano.Crypto.Wallet qualified as Crypto
import Codec.Serialise (Serialise)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Ledger.Address.Orphans as Export ()
import Ledger.Common (LedgerAddress)
import Ledger.Crypto (PubKey (PubKey), PubKeyHash (PubKeyHash), pubKeyHash, toPublicKey)
import Ledger.Orphans ()
import Ledger.Scripts (toCardanoScriptHash)
import Plutus.Script.Utils.Scripts qualified as PSU
import PlutusLedgerApi.V4 qualified as Api
import PlutusTx qualified
import PlutusTx.Lift (makeLift)
import PlutusTx.Prelude qualified as PlutusTx
import Prettyprinter (Pretty)

fromByronAddress :: Byron.Address -> Api.Credential
fromByronAddress = Api.PubKeyCredential . PubKeyHash . PlutusTx.toBuiltin . addrToBase58

fromKeyHash :: (C.SerialiseAsRawBytes (C.Hash key)) => C.Hash key -> PubKeyHash
fromKeyHash = PubKeyHash . PlutusTx.toBuiltin . C.serialiseToRawBytes

fromScriptHash :: C.ScriptHash -> Api.ScriptHash
fromScriptHash = Api.ScriptHash . PlutusTx.toBuiltin . C.serialiseToRawBytes

fromShelleyCredential :: C.Credential C.Payment -> Api.Credential
fromShelleyCredential =
  ( \case
      C.PaymentCredentialByKey paymentKeyHash -> Api.PubKeyCredential $ fromKeyHash paymentKeyHash
      C.PaymentCredentialByScript scriptHash -> Api.ScriptCredential $ fromScriptHash scriptHash
  )
    . C.fromShelleyPaymentCredential

fromCardanoStakeCredential :: C.StakeCredential -> Api.Credential
fromCardanoStakeCredential (C.StakeCredentialByKey stakeKeyHash) = Api.PubKeyCredential $ fromKeyHash stakeKeyHash
fromCardanoStakeCredential (C.StakeCredentialByScript scriptHash) = Api.ScriptCredential $ fromScriptHash scriptHash

fromCardanoAddress :: LedgerAddress -> Api.Address
fromCardanoAddress (C.AddressInEra C.ByronAddressInAnyEra (C.ByronAddress address)) =
  Api.Address (fromByronAddress address) Nothing
fromCardanoAddress (C.AddressInEra _ (C.ShelleyAddress _ paymentCredential (C.fromShelleyStakeReference -> C.StakeAddressByValue stakeCredential))) =
  Api.Address (fromShelleyCredential paymentCredential) (Just (Api.AccountId $ fromCardanoStakeCredential stakeCredential))
fromCardanoAddress (C.AddressInEra _ (C.ShelleyAddress _ paymentCredential _)) =
  Api.Address (fromShelleyCredential paymentCredential) Nothing

newtype PaymentPrivateKey = PaymentPrivateKey {unPaymentPrivateKey :: Crypto.XPrv}

newtype PaymentPubKey = PaymentPubKey {unPaymentPubKey :: PubKey}
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON, ToJSONKey, FromJSONKey)
  deriving newtype (PlutusTx.Eq, PlutusTx.Ord, Serialise, PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)
  deriving (Show, Pretty) via PubKey

makeLift ''PaymentPubKey

xprvToPaymentPubKey :: Crypto.XPrv -> PaymentPubKey
xprvToPaymentPubKey = PaymentPubKey . toPublicKey

newtype PaymentPubKeyHash = PaymentPubKeyHash {unPaymentPubKeyHash :: PubKeyHash}
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON, ToJSONKey, FromJSONKey)
  deriving newtype (PlutusTx.Eq, PlutusTx.Ord, Serialise, Hashable, PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)
  deriving (Show, Pretty) via PubKeyHash

makeLift ''PaymentPubKeyHash

xprvToPaymentPubKeyHash :: Crypto.XPrv -> PaymentPubKeyHash
xprvToPaymentPubKeyHash = PaymentPubKeyHash . pubKeyHash . toPublicKey

newtype StakePrivateKey = StakePrivateKey {unStakePrivateKey :: Crypto.XPrv}

newtype StakePubKey = StakePubKey {unStakePubKey :: PubKey}
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON, ToJSONKey, FromJSONKey)
  deriving newtype (PlutusTx.Eq, PlutusTx.Ord, Serialise, PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)
  deriving (Show, Pretty) via PubKey

makeLift ''StakePubKey

xprvToStakePubKey :: Crypto.XPrv -> StakePubKey
xprvToStakePubKey = StakePubKey . toPublicKey

newtype StakePubKeyHash = StakePubKeyHash {unStakePubKeyHash :: PubKeyHash}
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON, ToJSONKey, FromJSONKey)
  deriving newtype (PlutusTx.Eq, PlutusTx.Ord, Serialise, Hashable, PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)
  deriving (Show, Pretty) via PubKeyHash

makeLift ''StakePubKeyHash

xprvToStakePubKeyHash :: Crypto.XPrv -> StakePubKeyHash
xprvToStakePubKeyHash = StakePubKeyHash . pubKeyHash . toPublicKey

xprvToAccountId :: Crypto.XPrv -> Api.AccountId
xprvToAccountId = stakePubKeyHashCredential . xprvToStakePubKeyHash

{-# INLINEABLE paymentPubKeyHash #-}
paymentPubKeyHash :: PaymentPubKey -> PaymentPubKeyHash
paymentPubKeyHash = PaymentPubKeyHash . pubKeyHash . unPaymentPubKey

-- | The address that should be targeted by a transaction output locked by the
-- given public payment key (with its staking credentials).
{-# INLINEABLE pubKeyHashAddress #-}
pubKeyHashAddress :: PaymentPubKeyHash -> Maybe Api.AccountId -> Api.Address
pubKeyHashAddress = Api.Address . Api.PubKeyCredential . unPaymentPubKeyHash

-- | The address that should be targeted by a transaction output locked by the
-- given public key.  (with its staking credentials).
{-# INLINEABLE pubKeyAddress #-}
pubKeyAddress :: PaymentPubKey -> Maybe Api.AccountId -> Api.Address
pubKeyAddress = Api.Address . Api.PubKeyCredential . pubKeyHash . unPaymentPubKey

-- | The address that should be used by a transaction output locked by the given
-- validator script (with its staking credentials).
{-# INLINEABLE scriptValidatorHashAddress #-}
scriptValidatorHashAddress :: PSU.ValidatorHash -> Maybe Api.AccountId -> Api.Address
scriptValidatorHashAddress = Api.Address . Api.ScriptCredential . Api.ScriptHash . PSU.getValidatorHash

-- | Construct a `AccountId` from a public key hash.
{-# INLINEABLE stakePubKeyHashCredential #-}
stakePubKeyHashCredential :: StakePubKeyHash -> Api.AccountId
stakePubKeyHashCredential = Api.AccountId . Api.PubKeyCredential . unStakePubKeyHash

-- | Construct a `AccountId` from a validator script hash.
{-# INLINEABLE stakeValidatorHashCredential #-}
stakeValidatorHashCredential :: PSU.StakeValidatorHash -> Api.AccountId
stakeValidatorHashCredential = Api.AccountId . Api.ScriptCredential . Api.ScriptHash . PSU.getStakeValidatorHash

class ToWitness a where
  toWitness :: a -> C.ShelleyWitnessSigningKey

instance ToWitness PaymentPrivateKey where
  toWitness = C.WitnessPaymentExtendedKey . C.PaymentExtendedSigningKey . unPaymentPrivateKey

instance ToWitness StakePrivateKey where
  toWitness = C.WitnessStakeExtendedKey . C.StakeExtendedSigningKey . unStakePrivateKey

class ToCardanoAddress a where
  toCardanoAddress :: C.NetworkId -> a -> LedgerAddress

instance (PSU.ToScript a) => ToCardanoAddress (PSU.Versioned a) where
  toCardanoAddress networkId =
    flip (C.makeShelleyAddressInEra C.shelleyBasedEra networkId) C.NoStakeAddress
      . C.PaymentCredentialByScript
      . toCardanoScriptHash
