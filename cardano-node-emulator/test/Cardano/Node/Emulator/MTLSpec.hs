{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Cardano.Node.Emulator.MTLSpec (tests) where

import Cardano.Api qualified as C
import Cardano.Api.Address qualified as C
import Cardano.Node.Emulator.API (
  nextSlot,
  payToAddress,
  submitUnbalancedTx,
 )
import Cardano.Node.Emulator.Generators qualified as E
import Cardano.Node.Emulator.Test (
  Options (initialDistribution),
  checkPredicateOptions,
  defaultOptions,
  hasValidatedTransactionCountOfTotal,
  renderLogs,
  runEmulatorM,
 )
import Control.Monad (void)
import Data.Map qualified as Map
import Data.Text.Lazy qualified as LText
import Data.Text.Lazy.Encoding qualified as Text
import Ledger.Address (CardanoAddress, PaymentPrivateKey, toWitness, unPaymentPubKeyHash)
import Ledger.CardanoWallet (knownMockWallet, paymentPubKeyHash)
import Ledger.Tx.CardanoAPI (CardanoBuildTx (CardanoBuildTx), toCardanoStakeKeyHash)
import Ledger.Value.CardanoAPI qualified as Value
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)

tests :: TestTree
tests =
  testGroup
    "Cardano.Node.Emulator.MTL"
    [ checkPredicateOptions options "submit empty tx" (hasValidatedTransactionCountOfTotal 1 1) $ do
        void $ submitUnbalancedTx mempty w1 [toWitness pk1] (CardanoBuildTx E.emptyTxBodyContent)
        nextSlot
    , checkPredicateOptions options "payToAddress" (hasValidatedTransactionCountOfTotal 1 1) $ do
        void $ payToAddress (w1, pk1) w2 (Value.adaValueOf 1)
        nextSlot
    , checkPredicateOptions
        options
        "payToAddress twice in one slot"
        (hasValidatedTransactionCountOfTotal 2 2)
        $ do
          void $ payToAddress (w1, pk1) w2 (Value.adaValueOf 1)
          void $ payToAddress (w2, pk2) w1 (Value.adaValueOf 1)
          nextSlot
    , checkPredicateOptions options "payToAddress in two slots" (hasValidatedTransactionCountOfTotal 2 2) $ do
        void $ payToAddress (w1, pk1) w2 (Value.adaValueOf 1)
        nextSlot
        void $ payToAddress (w1, pk1) w2 (Value.adaValueOf 1)
        nextSlot
    , goldenVsString
        "captures the log of payToAddress"
        "test/Cardano/Node/Emulator/golden/logs - payToAddress.txt"
        ( pure . Text.encodeUtf8 . LText.fromStrict . renderLogs . snd . snd . runEmulatorM options $ do
            void $ payToAddress (w1, pk1) w2 (Value.adaValueOf 1)
            nextSlot
            void $ payToAddress (w1, pk1) w2 (Value.adaValueOf 1)
            nextSlot
        )
    , checkPredicateOptions options "submit staking tx" (hasValidatedTransactionCountOfTotal 1 1) $ do
        let
          Right stakeKeyHash = toCardanoStakeKeyHash . unPaymentPubKeyHash . paymentPubKeyHash $ knownMockWallet 1
          stakeCred = C.StakeCredentialByKey stakeKeyHash
          stakeCert =
            C.makeStakeAddressRegistrationCertificate
              (C.StakeAddrRegistrationPreConway C.ShelleyToBabbageEraBabbage stakeCred)
          tx =
            E.emptyTxBodyContent
              { C.txCertificates = C.TxCertificates C.shelleyBasedEra [stakeCert] (C.BuildTxWith mempty)
              }
        void $ submitUnbalancedTx mempty w1 [toWitness pk1] (CardanoBuildTx tx)
        nextSlot
    ]

w1, w2 :: CardanoAddress
w1 : w2 : _ = E.knownAddresses

pk1, pk2 :: PaymentPrivateKey
pk1 : pk2 : _ = E.knownPaymentPrivateKeys

options :: Options state
options =
  defaultOptions
    { initialDistribution = Map.fromList [(w1, Value.adaValueOf 10), (w2, Value.adaValueOf 10)]
    }
