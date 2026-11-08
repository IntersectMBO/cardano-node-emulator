{-# LANGUAGE OverloadedStrings #-}

module JSONSpec (tests) where

import Data.Aeson qualified as JSON
import Plutus.Script.Utils.Scripts
  ( Language (PlutusV1),
    Validator (getValidator),
    Versioned (Versioned),
  )
import Plutus.Script.Utils.V1.Generators
  ( alwaysSucceedPolicy,
    alwaysSucceedValidator,
    alwaysSucceedValidatorHash,
    mkForwardingStakeValidator,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

-- | Encode a value to JSON and decode it back, checking that the result is
-- equal to the original value.
roundTrips :: (Eq a, JSON.ToJSON a, JSON.FromJSON a) => a -> Bool
roundTrips x = JSON.decode (JSON.encode x) == Just x

tests :: TestTree
tests =
  testGroup
    "JSON instances"
    [ testCase "Script" $
        assertBool "roundtrip" (roundTrips (getValidator alwaysSucceedValidator)),
      testCase "Validator" $
        assertBool "roundtrip" (roundTrips alwaysSucceedValidator),
      testCase "MintingPolicy" $
        assertBool "roundtrip" (roundTrips alwaysSucceedPolicy),
      testCase "StakeValidator" $
        assertBool "roundtrip" (roundTrips (mkForwardingStakeValidator alwaysSucceedValidatorHash)),
      testCase "Versioned Validator" $
        assertBool "roundtrip" (roundTrips (Versioned alwaysSucceedValidator PlutusV1))
    ]
