{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wmissing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Ledger.Tx.CardanoAPISpec (tests) where

import Cardano.Api
  ( AsType (AsPaymentKey, AsStakeKey),
    Key (verificationKeyHash),
    NetworkId (Mainnet, Testnet),
    NetworkMagic (NetworkMagic),
    PaymentCredential (PaymentCredentialByKey),
    StakeAddressReference (NoStakeAddress, StakeAddressByValue),
    StakeCredential,
    makeShelleyAddress,
    shelleyAddressInEra,
  )
import Cardano.Api qualified as C
import Cardano.Api.Shelley (StakeCredential (StakeCredentialByKey), shelleyBasedEra)
import Cardano.Api.Shelley qualified as C
import GHC.Exts (fromList)
import Hedgehog (Gen, Property, forAll, property, tripping, (===))
import Hedgehog qualified
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Ledger (toPlutusAddress)
import Ledger.Tx.CardanoAPI
  ( fromCardanoAssetName,
    fromCardanoTxId,
    fromCardanoValue,
    toCardanoAddressInEra,
    toCardanoAssetName,
    toCardanoTxId,
    toCardanoValue,
  )
import Ledger.Value.CardanoAPI (combine, valueGeq)
import PlutusTx.Lattice ((\/))
import Test.Gen.Cardano.Api.Typed (genAssetName, genTxId)
import Test.Gen.Cardano.Api.Typed qualified as Gen
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

tests :: TestTree
tests =
  testGroup
    "CardanoAPI"
    [ testGroup
        "Ledger.Tx.CardanoAPI"
        [ testPropertyNamed
            "Cardano Address -> Plutus Address roundtrip"
            "addressRoundTripSpec"
            addressRoundTripSpec,
          testPropertyNamed
            "TokenName <- Cardano AssetName roundtrip"
            "cardanoAssetNameRoundTrip"
            cardanoAssetNameRoundTrip,
          testPropertyNamed
            "Plutus Value <- Cardano Value roundtrip"
            "cardanoValueRoundTrip"
            cardanoValueRoundTrip,
          testPropertyNamed "TxId round trip" "cardanoValueRoundTrip" cardanoTxIdRoundTrip
        ],
      testGroup
        "Ledger.Value.CardanoAPI"
        [ testPropertyNamed "combineLeftId" "combineLeftId" combineLeftId,
          testPropertyNamed "combineRightId" "combineRightId" combineRightId,
          testPropertyNamed "valueJoinGeq" "valueJoinGeq" valueJoinGeq
        ]
    ]

genValueDefault :: Gen C.Value
genValueDefault = C.fromMaryValue <$> Gen.genValueDefault C.MaryEraOnwardsConway

cardanoAssetNameRoundTrip :: Property
cardanoAssetNameRoundTrip = property $ do
  assetName <- forAll genAssetName
  tripping assetName fromCardanoAssetName toCardanoAssetName

cardanoValueRoundTrip :: Property
cardanoValueRoundTrip = property $ do
  value <- forAll genValueDefault
  tripping value fromCardanoValue toCardanoValue

cardanoTxIdRoundTrip :: Property
cardanoTxIdRoundTrip = property $ do
  txId <- forAll genTxId
  tripping txId fromCardanoTxId toCardanoTxId

-- | From a cardano address, we should be able to convert it to a plutus address,
-- back to the same initial cardano address.
addressRoundTripSpec :: Property
addressRoundTripSpec = property $ do
  networkId <- forAll genNetworkId
  shelleyAddr <-
    shelleyAddressInEra shelleyBasedEra
      <$> forAll
        ( makeShelleyAddress networkId
            <$> genPaymentCredential
            <*> genStakeAddressReference
        )
  let plutusAddr = toPlutusAddress shelleyAddr
  case toCardanoAddressInEra networkId plutusAddr of
    Left _ -> Hedgehog.assert False
    Right cAddr -> cAddr === shelleyAddr

-- Copied from Gen.Cardano.Api.Typed, because it's not exported.
genPaymentCredential :: Gen PaymentCredential
genPaymentCredential = do
  vKey <- Gen.genVerificationKey AsPaymentKey
  return . PaymentCredentialByKey $ verificationKeyHash vKey

-- Copied from Gen.Cardano.Api.Typed, because it's not exported.
genStakeAddressReference :: Gen StakeAddressReference
genStakeAddressReference =
  Gen.choice
    [ StakeAddressByValue <$> genStakeCredential,
      return NoStakeAddress
    ]

genStakeCredential :: Gen StakeCredential
genStakeCredential = do
  vKey <- Gen.genVerificationKey AsStakeKey
  return . StakeCredentialByKey $ verificationKeyHash vKey

-- Copied from Gen.Cardano.Api.Typed, because it's not exported.
genNetworkId :: Gen NetworkId
genNetworkId =
  Gen.choice
    [ pure Mainnet,
      Testnet <$> genNetworkMagic
    ]

-- Copied from Gen.Cardano.Api.Typed, because it's not exported.
genNetworkMagic :: Gen NetworkMagic
genNetworkMagic = NetworkMagic <$> Gen.word32 Range.constantBounded

combineLeftId :: Property
combineLeftId = property $ do
  valueL <- forAll genValueDefault
  valueR <- forAll genValueDefault
  combine (\a l _ -> fromList [(a, l)]) valueL valueR === valueL

combineRightId :: Property
combineRightId = property $ do
  valueL <- forAll genValueDefault
  valueR <- forAll genValueDefault
  combine (\a _ r -> fromList [(a, r)]) valueL valueR === valueR

valueJoinGeq :: Property
valueJoinGeq = property $ do
  valueL <- forAll genValueDefault
  valueR <- forAll genValueDefault
  let jn = valueL \/ valueR
  Hedgehog.annotateShow (valueL, valueR, jn)
  Hedgehog.assert (jn `valueGeq` valueL)
  Hedgehog.assert (jn `valueGeq` valueR)
