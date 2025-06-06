{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ledger.Address
  ( module Export,
    CardanoAddress,
    PaymentPrivateKey (..),
    PaymentPubKey (..),
    PaymentPubKeyHash (..),
    StakePrivateKey (..),
    StakePubKey (..),
    StakePubKeyHash (..),
    ToWitness (..),
    toPlutusAddress,
    toPlutusPubKeyHash,
    cardanoAddressCredential,
    cardanoPubKeyHash,
    cardanoStakingCredential,
    paymentPubKeyHash,
    pubKeyHashAddress,
    pubKeyAddress,
    scriptValidatorHashAddress,
    stakePubKeyHashCredential,
    stakeValidatorHashCredential,
    xprvToPaymentPubKey,
    xprvToPaymentPubKeyHash,
    xprvToStakingCredential,
    xprvToStakePubKey,
    xprvToStakePubKeyHash,
  )
where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.Chain.Common (addrToBase58)
import Cardano.Crypto.Wallet qualified as Crypto
import Codec.Serialise (Serialise)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Ledger.Address.Orphans as Export ()
import Ledger.Crypto (PubKey (PubKey), PubKeyHash (PubKeyHash), pubKeyHash, toPublicKey)
import Ledger.Orphans ()
import Ledger.Scripts
  ( ScriptHash (..),
    StakeValidatorHash (..),
    ValidatorHash (..),
  )
import PlutusLedgerApi.V1.Address as Export hiding (pubKeyHashAddress, toScriptHash)
import PlutusLedgerApi.V1.Credential
  ( Credential (PubKeyCredential, ScriptCredential),
    StakingCredential (StakingHash),
  )
import PlutusTx qualified
import PlutusTx.Lift (makeLift)
import PlutusTx.Prelude qualified as PlutusTx
import Prettyprinter (Pretty)

type CardanoAddress = C.AddressInEra C.ConwayEra

instance ToJSONKey (C.AddressInEra C.ConwayEra)

instance FromJSONKey (C.AddressInEra C.ConwayEra)

cardanoAddressCredential :: C.AddressInEra era -> Credential
cardanoAddressCredential (C.AddressInEra C.ByronAddressInAnyEra (C.ByronAddress address)) =
  PubKeyCredential $
    PubKeyHash $
      PlutusTx.toBuiltin $
        addrToBase58 address
cardanoAddressCredential (C.AddressInEra _ (C.ShelleyAddress _ paymentCredential _)) =
  case C.fromShelleyPaymentCredential paymentCredential of
    C.PaymentCredentialByKey paymentKeyHash ->
      PubKeyCredential $
        PubKeyHash $
          PlutusTx.toBuiltin $
            C.serialiseToRawBytes paymentKeyHash
    C.PaymentCredentialByScript scriptHash ->
      ScriptCredential $ scriptToScriptHash scriptHash

cardanoStakingCredential :: C.AddressInEra era -> Maybe StakingCredential
cardanoStakingCredential (C.AddressInEra C.ByronAddressInAnyEra _) = Nothing
cardanoStakingCredential (C.AddressInEra _ (C.ShelleyAddress _ _ stakeAddressReference)) =
  case C.fromShelleyStakeReference stakeAddressReference of
    C.NoStakeAddress -> Nothing
    (C.StakeAddressByValue stakeCredential) ->
      Just (StakingHash $ fromCardanoStakeCredential stakeCredential)
    C.StakeAddressByPointer {} -> Nothing -- Not supported
  where
    fromCardanoStakeCredential :: C.StakeCredential -> Credential
    fromCardanoStakeCredential (C.StakeCredentialByKey stakeKeyHash) =
      PubKeyCredential $
        PubKeyHash $
          PlutusTx.toBuiltin $
            C.serialiseToRawBytes stakeKeyHash
    fromCardanoStakeCredential (C.StakeCredentialByScript scriptHash) = ScriptCredential (scriptToScriptHash scriptHash)

cardanoPubKeyHash :: C.AddressInEra era -> Maybe PubKeyHash
cardanoPubKeyHash addr = case cardanoAddressCredential addr of
  PubKeyCredential x -> Just x
  _ -> Nothing

toPlutusAddress :: C.AddressInEra era -> Address
toPlutusAddress address = Address (cardanoAddressCredential address) (cardanoStakingCredential address)

toPlutusPubKeyHash :: C.Hash C.PaymentKey -> PubKeyHash
toPlutusPubKeyHash paymentKeyHash = PubKeyHash $ PlutusTx.toBuiltin $ C.serialiseToRawBytes paymentKeyHash

scriptToScriptHash :: C.ScriptHash -> ScriptHash
scriptToScriptHash = ScriptHash . PlutusTx.toBuiltin . C.serialiseToRawBytes

newtype PaymentPrivateKey = PaymentPrivateKey {unPaymentPrivateKey :: Crypto.XPrv}

newtype PaymentPubKey = PaymentPubKey {unPaymentPubKey :: PubKey}
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON, ToJSONKey, FromJSONKey)
  deriving newtype
    (PlutusTx.Eq, PlutusTx.Ord, Serialise, PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)
  deriving (Show, Pretty) via PubKey

makeLift ''PaymentPubKey

xprvToPaymentPubKey :: Crypto.XPrv -> PaymentPubKey
xprvToPaymentPubKey = PaymentPubKey . toPublicKey

newtype PaymentPubKeyHash = PaymentPubKeyHash {unPaymentPubKeyHash :: PubKeyHash}
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON, ToJSONKey, FromJSONKey)
  deriving newtype
    ( PlutusTx.Eq,
      PlutusTx.Ord,
      Serialise,
      Hashable,
      PlutusTx.ToData,
      PlutusTx.FromData,
      PlutusTx.UnsafeFromData
    )
  deriving (Show, Pretty) via PubKeyHash

makeLift ''PaymentPubKeyHash

xprvToPaymentPubKeyHash :: Crypto.XPrv -> PaymentPubKeyHash
xprvToPaymentPubKeyHash = PaymentPubKeyHash . pubKeyHash . toPublicKey

newtype StakePrivateKey = StakePrivateKey {unStakePrivateKey :: Crypto.XPrv}

newtype StakePubKey = StakePubKey {unStakePubKey :: PubKey}
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON, ToJSONKey, FromJSONKey)
  deriving newtype
    (PlutusTx.Eq, PlutusTx.Ord, Serialise, PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)
  deriving (Show, Pretty) via PubKey

makeLift ''StakePubKey

xprvToStakePubKey :: Crypto.XPrv -> StakePubKey
xprvToStakePubKey = StakePubKey . toPublicKey

newtype StakePubKeyHash = StakePubKeyHash {unStakePubKeyHash :: PubKeyHash}
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON, ToJSONKey, FromJSONKey)
  deriving newtype
    ( PlutusTx.Eq,
      PlutusTx.Ord,
      Serialise,
      Hashable,
      PlutusTx.ToData,
      PlutusTx.FromData,
      PlutusTx.UnsafeFromData
    )
  deriving (Show, Pretty) via PubKeyHash

makeLift ''StakePubKeyHash

xprvToStakePubKeyHash :: Crypto.XPrv -> StakePubKeyHash
xprvToStakePubKeyHash = StakePubKeyHash . pubKeyHash . toPublicKey

xprvToStakingCredential :: Crypto.XPrv -> StakingCredential
xprvToStakingCredential = stakePubKeyHashCredential . xprvToStakePubKeyHash

{-# INLINEABLE paymentPubKeyHash #-}
paymentPubKeyHash :: PaymentPubKey -> PaymentPubKeyHash
paymentPubKeyHash (PaymentPubKey pk) = PaymentPubKeyHash (pubKeyHash pk)

{-# INLINEABLE pubKeyHashAddress #-}

-- | The address that should be targeted by a transaction output locked by the
-- given public payment key (with its staking credentials).
pubKeyHashAddress :: PaymentPubKeyHash -> Maybe StakingCredential -> Address
pubKeyHashAddress (PaymentPubKeyHash pkh) = Address (PubKeyCredential pkh)

{-# INLINEABLE pubKeyAddress #-}

-- | The address that should be targeted by a transaction output locked by the given public key.
-- (with its staking credentials).
pubKeyAddress :: PaymentPubKey -> Maybe StakingCredential -> Address
pubKeyAddress (PaymentPubKey pk) = Address (PubKeyCredential (pubKeyHash pk))

{-# INLINEABLE scriptValidatorHashAddress #-}

-- | The address that should be used by a transaction output locked by the given validator script
-- (with its staking credentials).
scriptValidatorHashAddress :: ValidatorHash -> Maybe StakingCredential -> Address
scriptValidatorHashAddress (ValidatorHash vh) = Address (ScriptCredential (ScriptHash vh))

{-# INLINEABLE stakePubKeyHashCredential #-}

-- | Construct a `StakingCredential` from a public key hash.
stakePubKeyHashCredential :: StakePubKeyHash -> StakingCredential
stakePubKeyHashCredential = StakingHash . PubKeyCredential . unStakePubKeyHash

{-# INLINEABLE stakeValidatorHashCredential #-}

-- | Construct a `StakingCredential` from a validator script hash.
stakeValidatorHashCredential :: StakeValidatorHash -> StakingCredential
stakeValidatorHashCredential (StakeValidatorHash h) = StakingHash . ScriptCredential . ScriptHash $ h

class ToWitness a where
  toWitness :: a -> C.ShelleyWitnessSigningKey

instance ToWitness PaymentPrivateKey where
  toWitness (PaymentPrivateKey xprv) = C.WitnessPaymentExtendedKey (C.PaymentExtendedSigningKey xprv)

instance ToWitness StakePrivateKey where
  toWitness (StakePrivateKey xprv) = C.WitnessStakeExtendedKey (C.StakeExtendedSigningKey xprv)
