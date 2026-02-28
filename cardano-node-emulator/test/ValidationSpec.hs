{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as Shelley
import Cardano.Node.Emulator.Internal.Node.Validation
  (validateAndApplyTx, initialState)
import Cardano.Node.Emulator.Generators (knownAddresses, makeTx)
import Data.Default (def)
import Data.Either (isLeft)
import Data.Maybe (fromJust)
import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (testCase, assertBool)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "validation tests"
    [ testCase "duplicate inputs cause failure" duplicateTest,
      testCase "oversize tx with duplicates also fails" oversizeTest
    ]

-- use a fixed dummy tx id so that transaction bodies are well-formed
dummyTxId :: C.TxId
dummyTxId =
  fromJust
    $ C.deserialiseFromRawBytesHex
      C.AsTxId
      "0000000000000000000000000000000000000000000000000000000000000000"

mkMalformedTx :: Int -> C.Tx C.ConwayEra
mkMalformedTx nInputs =
  let input = C.TxIn dummyTxId (C.TxIx 0)
      witness = C.BuildTxWith $ C.KeyWitness C.KeyWitnessForSpending
      txIns = replicate nInputs (input, witness)
      addr = head knownAddresses
      txOut =
        C.TxOut
          (C.AddressInEra (C.ShelleyAddressInEra C.ShelleyBasedEraConway) addr)
          (C.lovelaceToValue 1_000_000)
          C.TxOutDatumNone
      body =
        C.TxBodyContent
          { C.txIns = txIns,
            C.txOuts = [txOut],
            C.txFee = C.TxFeeExplicit C.TxFeesExplicitInConwayEra 0,
            C.txValidityRange =
              ( C.TxValidityNoLowerBound,
                C.TxValidityNoUpperBound C.TxValidityUpperBoundInConwayEra 1000
              ),
            C.txMetadata = C.TxMetadataNone,
            C.txAuxScripts = C.TxAuxScriptsNone,
            C.txCertificates = C.TxCertificatesNone,
            C.txWithdrawals = C.TxWithdrawalsNone,
            C.txProtocolParams = Nothing,
            C.txUpdateProposal = C.TxUpdateProposalNone,
            C.txMintValue = C.TxMintNone,
            C.txScriptValidity = C.TxScriptValidityNone
          }
   in case C.createTransactionBody C.shelleyBasedEra body of
        Left err -> error $ "mkMalformedTx: " ++ show err
        Right body' -> C.ShelleyTx Shelley.ShelleyBasedEraConway body'

duplicateTest :: IO ()
duplicateTest =
  assertBool
    "transaction with duplicated input should be rejected"
    ( isLeft
        ( validateAndApplyTx
            def
            (initialState def)
            (mkMalformedTx 2)
        )
    )

oversizeTest :: IO ()
oversizeTest =
  assertBool
    "large transaction with repeated inputs should be rejected"
    ( isLeft
        ( validateAndApplyTx
            def
            (initialState def)
            (mkMalformedTx 200)
        )
    )
