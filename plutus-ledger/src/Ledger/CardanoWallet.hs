{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{- Cardano wallet implementation for the emulator.
-}
module Ledger.CardanoWallet
  ( MockWallet (..),

    -- * Enumerating wallets
    WalletNumber (..),
    fromWalletNumber,
    toWalletNumber,
    knownMockWallets,
    knownMockWallet,
    fromSeed,
    fromSeed',

    -- ** Keys
    mockWalletAddress,
    paymentPrivateKey,
    paymentPubKeyHash,
    paymentPubKey,
    stakingCredential,
    stakePubKeyHash,
    stakePubKey,
    stakePrivateKey,
    knownAddresses,
    knownPaymentKeys,
    knownPaymentPublicKeys,
    knownPaymentPrivateKeys,
  )
where

import Cardano.Crypto.Wallet qualified as Crypto
import Codec.Serialise (serialise)
import Crypto.Hash qualified as Crypto
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.Extras (encodeByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Coerce (coerce)
import Data.Either (fromRight)
import Data.Hashable (Hashable (..))
import Data.List (findIndex)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Ledger.Address
  ( CardanoAddress,
    PaymentPrivateKey (PaymentPrivateKey, unPaymentPrivateKey),
    PaymentPubKey (PaymentPubKey, unPaymentPubKey),
    PaymentPubKeyHash (PaymentPubKeyHash, unPaymentPubKeyHash),
    StakePrivateKey (StakePrivateKey, unStakePrivateKey),
    StakePubKey (StakePubKey, unStakePubKey),
    StakePubKeyHash (StakePubKeyHash, unStakePubKeyHash),
    stakePubKeyHashCredential,
  )
import Ledger.Crypto (PubKey (..))
import Ledger.Crypto qualified as Crypto
import Ledger.Test (testnet)
import Ledger.Tx.CardanoAPI.Internal qualified as Tx
import PlutusLedgerApi.V1
  ( Address (Address),
    Credential (PubKeyCredential),
    StakingCredential (StakingHash),
  )
import PlutusLedgerApi.V1.Bytes (LedgerBytes (getLedgerBytes))
import Servant.API (FromHttpApiData, ToHttpApiData)

newtype MockPrivateKey = MockPrivateKey {unMockPrivateKey :: Crypto.XPrv}

instance Show MockPrivateKey where
  show = T.unpack . encodeByteString . Crypto.unXPrv . unMockPrivateKey

instance Eq MockPrivateKey where
  (MockPrivateKey l) == (MockPrivateKey r) = Crypto.unXPrv l == Crypto.unXPrv r

instance Ord MockPrivateKey where
  compare (MockPrivateKey l) (MockPrivateKey r) = compare (Crypto.unXPrv l) (Crypto.unXPrv r)

instance Hashable MockPrivateKey where
  hashWithSalt i = hashWithSalt i . Crypto.unXPrv . unMockPrivateKey

-- | Emulated wallet with a key and a passphrase
data MockWallet = MockWallet
  { mwWalletId :: Crypto.Digest Crypto.Blake2b_160,
    mwPaymentKey :: MockPrivateKey,
    mwStakeKey :: Maybe MockPrivateKey,
    mwPrintAs :: Maybe String
  }
  deriving (Show)

-- | Wrapper for config files and APIs
newtype WalletNumber = WalletNumber {getWallet :: Integer}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (ToHttpApiData, FromHttpApiData, Num, Enum, Real)
  deriving anyclass (FromJSON, ToJSON)

-- Workaround for warning "Call of toInteger :: Integer -> Integer can probably be omitted" GHC issue #21679
instance Integral WalletNumber where
  quotRem =
    coerce @(Integer -> Integer -> (Integer, Integer))
      @(WalletNumber -> WalletNumber -> (WalletNumber, WalletNumber))
      quotRem
  toInteger = coerce

fromWalletNumber :: WalletNumber -> MockWallet
fromWalletNumber (WalletNumber i) = (fromSeed' (BSL.toStrict $ serialise i)) {mwPrintAs = Just (show i)}

fromSeed :: BS.ByteString -> Crypto.Passphrase -> MockWallet
fromSeed bs passPhrase = fromSeedInternal (flip Crypto.generateFromSeed passPhrase) bs

fromSeed' :: BS.ByteString -> MockWallet
fromSeed' = fromSeedInternal Crypto.generateFromSeed'

fromSeedInternal :: (BS.ByteString -> Crypto.XPrv) -> BS.ByteString -> MockWallet
fromSeedInternal seedGen bs = MockWallet {mwWalletId, mwPaymentKey, mwStakeKey, mwPrintAs = Nothing}
  where
    missing = max 0 (32 - BS.length bs)
    bs' = bs <> BS.replicate missing 0
    k = seedGen bs'
    mwWalletId =
      fromMaybe (error "Ledger.CardanoWallet.fromSeed: digestFromByteString") $
        Crypto.digestFromByteString $
          Crypto.hashWith Crypto.Blake2b_160 $
            getLedgerBytes $
              getPubKey $
                Crypto.toPublicKey k
    mwPaymentKey = MockPrivateKey k
    mwStakeKey = Nothing

toWalletNumber :: MockWallet -> WalletNumber
toWalletNumber MockWallet {mwWalletId = w} =
  maybe
    (error "Ledger.CardanoWallet.toWalletNumber: not a known wallet")
    (WalletNumber . toInteger . succ)
    $ findIndex ((==) w . mwWalletId) knownMockWallets

-- | The wallets used in mockchain simulations by default. There are
--  ten wallets by default.
knownMockWallets :: [MockWallet]
knownMockWallets = fromWalletNumber . WalletNumber <$> [1 .. 10]

-- | Get a known wallet from an @Integer@ indexed from 1 to 10.
knownMockWallet :: Integer -> MockWallet
knownMockWallet = (knownMockWallets !!) . pred . fromInteger

-- | A mock cardano address for the testnet network.
mockWalletAddress :: MockWallet -> CardanoAddress
mockWalletAddress =
  fromRight (error "mock wallet is invalid")
    . Tx.toCardanoAddressInEra testnet
    . plutusAddress
  where
    plutusAddress mw =
      Address
        (PubKeyCredential $ unPaymentPubKeyHash $ paymentPubKeyHash mw)
        (StakingHash . PubKeyCredential . unStakePubKeyHash <$> stakePubKeyHash mw)

-- | Mock wallet's private key
paymentPrivateKey :: MockWallet -> PaymentPrivateKey
paymentPrivateKey = PaymentPrivateKey . unMockPrivateKey . mwPaymentKey

-- | The mock wallet's public key hash
paymentPubKeyHash :: MockWallet -> PaymentPubKeyHash
paymentPubKeyHash = PaymentPubKeyHash . Crypto.pubKeyHash . unPaymentPubKey . paymentPubKey

-- | The mock wallet's payment public key
paymentPubKey :: MockWallet -> PaymentPubKey
paymentPubKey = PaymentPubKey . Crypto.toPublicKey . unMockPrivateKey . mwPaymentKey

-- | The mock wallet's stake public key hash
stakePubKeyHash :: MockWallet -> Maybe StakePubKeyHash
stakePubKeyHash w = StakePubKeyHash . Crypto.pubKeyHash . unStakePubKey <$> stakePubKey w

-- | The mock wallet's stake public key
stakePubKey :: MockWallet -> Maybe StakePubKey
stakePubKey w = StakePubKey . Crypto.toPublicKey . unStakePrivateKey <$> stakePrivateKey w

-- | The mock wallet's stake private key
stakePrivateKey :: MockWallet -> Maybe StakePrivateKey
stakePrivateKey w = StakePrivateKey . unMockPrivateKey <$> mwStakeKey w

-- | The mock wallet's staking credentials
stakingCredential :: MockWallet -> Maybe StakingCredential
stakingCredential = fmap stakePubKeyHashCredential . stakePubKeyHash

knownPaymentPublicKeys :: [PaymentPubKey]
knownPaymentPublicKeys =
  PaymentPubKey . Crypto.toPublicKey . unPaymentPrivateKey <$> knownPaymentPrivateKeys

knownPaymentKeys :: Map.Map PaymentPubKey PaymentPrivateKey
knownPaymentKeys =
  Map.fromList $
    map
      (\k -> (PaymentPubKey $ Crypto.toPublicKey $ unPaymentPrivateKey k, k))
      knownPaymentPrivateKeys

knownPaymentPrivateKeys :: [PaymentPrivateKey]
knownPaymentPrivateKeys = paymentPrivateKey <$> knownMockWallets

knownAddresses :: [CardanoAddress]
knownAddresses = mockWalletAddress <$> knownMockWallets
