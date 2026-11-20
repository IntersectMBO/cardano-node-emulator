{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Ledger.CardanoPool where

import Cardano.Api
  ( AsType (AsKesKey, AsStakePoolKey, AsVrfKey),
    HasTypeProxy,
    KesKey,
    Key,
    PoolId,
    SigningKey,
    StakePoolKey,
    VrfKey,
    deterministicSigningKey,
    deterministicSigningKeySeedSize,
    getVerificationKey,
    serialiseToBech32,
    verificationKeyHash,
  )
import Cardano.Crypto.Seed (mkSeedFromBytes)
import Data.Text (Text)
import GHC.Generics (Generic)
import System.Random (StdGen, mkStdGen)
import System.Random.Stateful (runStateGen, uniformByteStringM)

data MockPoolKeys = MockPoolKeys
  { mpkStakePoolKey :: SigningKey StakePoolKey,
    mpkVrfKey :: SigningKey VrfKey,
    mpkKesKey :: SigningKey KesKey
  }
  deriving stock (Generic, Show)

data MockPool = MockPool
  { mpPoolKeys :: MockPoolKeys,
    mpPoolId :: PoolId
  }
  deriving stock (Generic, Show)

newtype PoolNumber = PoolNumber {getPool :: Int}
  deriving stock (Show, Eq, Ord, Generic)

pureGenKey :: (Key k, HasTypeProxy k) => StdGen -> AsType k -> (SigningKey k, StdGen)
pureGenKey g keytype =
  let (bs, g') = runStateGen g (uniformByteStringM (fromIntegral (deterministicSigningKeySeedSize keytype)))
      seed = mkSeedFromBytes bs
   in (deterministicSigningKey keytype seed, g')

genPoolKeys :: PoolNumber -> MockPoolKeys
genPoolKeys (PoolNumber pn) =
  let g0 = mkStdGen pn
      (coldkey, g1) = pureGenKey g0 AsStakePoolKey
      (vrfKey, g2) = pureGenKey g1 AsVrfKey
      (kesKey, _) = pureGenKey g2 AsKesKey
   in MockPoolKeys coldkey vrfKey kesKey

genPool :: PoolNumber -> MockPool
genPool pn =
  let keys = genPoolKeys pn
      poolId = verificationKeyHash (getVerificationKey (mpkStakePoolKey keys))
   in MockPool keys poolId

knownMockPools :: [MockPool]
knownMockPools = genPool . PoolNumber <$> [1 .. 10]

knownMockPool :: Int -> MockPool
knownMockPool = (knownMockPools !!) . pred

poolIdBech32 :: PoolId -> Text
poolIdBech32 = serialiseToBech32
