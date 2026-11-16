{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ledger.Common
  ( LedgerEra,
    LedgerAddress,
    testNetworkMagic,
    testnet,
  )
where

import Cardano.Api qualified as C
import Data.Aeson (FromJSONKey, ToJSONKey)

-- | The current era of the codebase
type LedgerEra = C.DijkstraEra

-- | The Cardano address associated with the current era
type LedgerAddress = C.AddressInEra LedgerEra

instance ToJSONKey LedgerAddress

instance FromJSONKey LedgerAddress

testNetworkMagic :: C.NetworkMagic
testNetworkMagic = C.NetworkMagic 1097911063

testnet :: C.NetworkId
testnet = C.Testnet testNetworkMagic
