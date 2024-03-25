{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Plutus.CIP1694.Test where

import Cardano.Node.Emulator.API (
  nextSlot,
  submitUnbalancedTx,
 )
import Cardano.Node.Emulator.Generators qualified as E
import Cardano.Node.Emulator.Internal.Node.Params (
  emulatorShelleyGenesisDefaults,
  mkLatestTransitionConfig,
  paramsFromConfig,
 )
import Cardano.Node.Emulator.Test (
  Options (initialDistribution, params),
  checkPredicateOptionsIO,
  defaultOptions,
  hasValidatedTransactionCountOfTotal,
 )
import Data.Aeson (FromJSON, eitherDecode)
import Data.ByteString.Lazy qualified as BSL
import Data.Functor (void)
import Data.Map qualified as Map
import Ledger.Address (CardanoAddress, PaymentPrivateKey, toWitness)
import Ledger.Tx.CardanoAPI (CardanoBuildTx (CardanoBuildTx))
import Ledger.Value.CardanoAPI qualified as Value
import Test.Tasty (TestTree, testGroup)

-- import Plutus.CIP1694.Scripts (ccScriptCode, lockingScriptCode)

w1, w2 :: CardanoAddress
w1 : w2 : _ = E.knownAddresses

pk1, pk2 :: PaymentPrivateKey
pk1 : pk2 : _ = E.knownPaymentPrivateKeys

tests :: TestTree
tests =
  testGroup
    "CIP-1694"
    [ checkPredicateOptionsIO options "submit empty tx" (hasValidatedTransactionCountOfTotal 1 1) $ do
        void $ submitUnbalancedTx mempty w1 [toWitness pk1] (CardanoBuildTx E.emptyTxBodyContent)
        nextSlot
    ]

options :: IO (Options state)
options = do
  alonzoConfig <- readConfig "test/Plutus/CIP1694/alonzo-babbage-test-genesis.json"
  conwayConfig <- readConfig "test/Plutus/CIP1694/conway-babbage-test-genesis.json"
  pure $
    defaultOptions
      { initialDistribution = Map.fromList [(w1, Value.adaValueOf 10), (w2, Value.adaValueOf 10)]
      , params =
          paramsFromConfig $
            mkLatestTransitionConfig emulatorShelleyGenesisDefaults alonzoConfig conwayConfig
      }

readConfig :: (FromJSON a) => FilePath -> IO a
readConfig path = do
  bs <- BSL.readFile path
  case eitherDecode bs of
    Left err ->
      error $
        "Error reading JSON file: "
          ++ show path
          ++ " ("
          ++ err
          ++ ")"
    Right params -> pure params
