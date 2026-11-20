{-# LANGUAGE NoImplicitPrelude #-}

module Plutus.Script.Utils.V1.Contexts
  ( outputsAt,
    valuePaidTo,
  )
where

import Plutus.Script.Utils.V1.Address (ToAddress (toAddress))
import PlutusLedgerApi.V1 (TxInfo (txInfoOutputs), TxOut (TxOut), Value)
import PlutusTx.Prelude (Maybe (Just, Nothing), mapMaybe, mconcat, (.), (==))

-- | Get the list of values paid to an address within a 'TxInfo'
{-# INLINEABLE outputsAt #-}
outputsAt :: (ToAddress a) => TxInfo -> a -> [Value]
outputsAt txInfo (toAddress -> addr) =
  mapMaybe
    (\(TxOut addr' val _) -> if addr == addr' then Just val else Nothing)
    (txInfoOutputs txInfo)

-- | Get the total value paid to an address within a 'TxInfo'
{-# INLINEABLE valuePaidTo #-}
valuePaidTo :: (ToAddress a) => TxInfo -> a -> Value
valuePaidTo txInfo = mconcat . outputsAt txInfo
