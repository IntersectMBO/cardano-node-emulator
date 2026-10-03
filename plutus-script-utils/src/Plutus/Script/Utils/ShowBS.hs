{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}

-- | This module exposes on-chain pretty-printing to 'BuiltinString' of the
-- types that are common to every version of the 'ScriptContext'. This is useful
-- for debugging of validators. You probably do not want to use this in
-- production code, as many of the functions in this module are wildly
-- inefficient due to limitations of the 'BuiltinString' type.
--
-- The version-specific instances (for the 'ScriptContext' of each Plutus
-- language version) live in the @Plutus.Script.Utils.VX.ShowBS@ modules, each of
-- which re-uses the instances defined here and in the lower versions.
module Plutus.Script.Utils.ShowBS
  ( ShowBS (..),
    showBSParen,
    application1,
    application2,
    application3,
    application4,
    catList,
    integerToDigits,
    digitToBS,
    builtinByteStringCharacters,
    showData,
  )
where

import PlutusLedgerApi.V1 qualified as V1
import PlutusTx.AssocMap qualified as Map
import PlutusTx.Builtins
import PlutusTx.Prelude

-- | Analogue of Haskell's 'Prelude.Show' class to be used in Plutus scripts.
class ShowBS a where
  -- | Analogue of 'Prelude.show'
  showBS :: a -> BuiltinString

-- | Print with a surrounding parenthesis
{-# INLINEABLE showBSParen #-}
showBSParen :: BuiltinString -> BuiltinString
showBSParen s = "(" <> s <> ")"

-- | Print an application of a constructor to an argument
{-# INLINEABLE application1 #-}
application1 :: (ShowBS a) => BuiltinString -> a -> BuiltinString
application1 bs x = showBSParen $ bs <> " " <> showBS x

-- | Like 'application1' with two arguments
{-# INLINEABLE application2 #-}
application2 :: (ShowBS a, ShowBS b) => BuiltinString -> a -> b -> BuiltinString
application2 bs x y = showBSParen $ bs <> " " <> showBS x <> " " <> showBS y

-- | Like 'application1' with three arguments
{-# INLINEABLE application3 #-}
application3 :: (ShowBS a, ShowBS b, ShowBS c) => BuiltinString -> a -> b -> c -> BuiltinString
application3 bs x y z = showBSParen $ bs <> " " <> showBS x <> " " <> showBS y <> " " <> showBS z

-- | Like 'application1' with four arguments
{-# INLINEABLE application4 #-}
application4 :: (ShowBS a, ShowBS b, ShowBS c, ShowBS d) => BuiltinString -> a -> b -> c -> d -> BuiltinString
application4 bs x y z w =
  showBSParen $ bs <> " " <> showBS x <> " " <> showBS y <> " " <> showBS z <> " " <> showBS w

instance ShowBS Integer where
  {-# INLINEABLE showBS #-}
  showBS i = mconcat (integerToDigits i)

{-# INLINEABLE integerToDigits #-}
integerToDigits :: Integer -> [BuiltinString]
integerToDigits n
  | n < 0 = "-" : go (negate n) []
  | n == 0 = ["0"]
  | otherwise = go n []
  where
    go i acc
      | i == 0 = acc
      | otherwise = let (q, r) = quotRem i 10 in go q (digitToBS r : acc)

{-# INLINEABLE digitToBS #-}
digitToBS :: Integer -> BuiltinString
digitToBS x
  | x == 0 = "0"
  | x == 1 = "1"
  | x == 2 = "2"
  | x == 3 = "3"
  | x == 4 = "4"
  | x == 5 = "5"
  | x == 6 = "6"
  | x == 7 = "7"
  | x == 8 = "8"
  | x == 9 = "9"
  | otherwise = "?"

instance (ShowBS a) => ShowBS [a] where
  {-# INLINEABLE showBS #-}
  showBS = catList "[" "," "]" showBS

{-# INLINEABLE catList #-}
catList :: BuiltinString -> BuiltinString -> BuiltinString -> (a -> BuiltinString) -> [a] -> BuiltinString
catList open _ close _ [] = open <> close
catList open sep close print (x : xs) = open <> print x <> printSeparated xs <> close
  where
    printSeparated [] = ""
    printSeparated (y : ys) = sep <> print y <> printSeparated ys

instance (ShowBS a, ShowBS b) => ShowBS (a, b) where
  {-# INLINEABLE showBS #-}
  showBS (x, y) = "(" <> showBS x <> "," <> showBS y <> ")"

instance ShowBS Bool where
  {-# INLINEABLE showBS #-}
  showBS True = "True"
  showBS False = "False"

instance (ShowBS a) => ShowBS (Maybe a) where
  {-# INLINEABLE showBS #-}
  showBS Nothing = "Nothing"
  showBS (Just x) = application1 "Just" x

instance (ShowBS k, ShowBS v) => ShowBS (Map.Map k v) where
  {-# INLINEABLE showBS #-}
  showBS m = application1 "fromList" (Map.toList m)

instance ShowBS BuiltinByteString where
  -- base16 representation
  {-# INLINEABLE showBS #-}
  showBS bs = "\"" <> mconcat (builtinByteStringCharacters bs) <> "\""

{-# INLINEABLE builtinByteStringCharacters #-}
builtinByteStringCharacters :: BuiltinByteString -> [BuiltinString]
builtinByteStringCharacters s = go (len - 1) []
  where
    len = lengthOfByteString s

    go :: Integer -> [BuiltinString] -> [BuiltinString]
    go i acc
      | i >= 0 =
          let (highNibble, lowNibble) = quotRem (indexByteString s i) 16
           in go (i - 1) (toHex highNibble : toHex lowNibble : acc)
      | otherwise = acc

    toHex :: Integer -> BuiltinString
    toHex x
      | x <= 9 = digitToBS x
      | x == 10 = "a"
      | x == 11 = "b"
      | x == 12 = "c"
      | x == 13 = "d"
      | x == 14 = "e"
      | x == 15 = "f"
      | otherwise = "?"

instance ShowBS BuiltinData where
  {-# INLINEABLE showBS #-}
  showBS d = showBSParen $ "BuiltinData " <> showData d

{-# INLINEABLE showData #-}
showData :: BuiltinData -> BuiltinString
showData d =
  matchData
    d
    (\i ds -> showBSParen $ "Constr " <> showBS i <> " " <> catList "[" "," "]" showData ds)
    (\alist -> showBSParen $ "Map " <> catList "[" "," "]" (\(a, b) -> "(" <> showData a <> "," <> showData b <> ")") alist)
    (\list -> showBSParen $ "List " <> catList "[" "," "]" showData list)
    (\i -> showBSParen $ "I " <> showBS i)
    (\bs -> showBSParen $ "B " <> showBS bs)

instance ShowBS V1.TokenName where
  {-# INLINEABLE showBS #-}
  showBS (V1.TokenName x) = application1 "TokenName" x

instance ShowBS V1.CurrencySymbol where
  {-# INLINEABLE showBS #-}
  showBS (V1.CurrencySymbol x) = application1 "CurrencySymbol" x

instance ShowBS V1.Value where
  {-# INLINEABLE showBS #-}
  showBS (V1.Value m) = application1 "Value" m

instance ShowBS V1.Lovelace where
  {-# INLINEABLE showBS #-}
  showBS (V1.Lovelace amount) = application1 "Lovelace" amount

instance ShowBS V1.PubKeyHash where
  {-# INLINEABLE showBS #-}
  showBS (V1.PubKeyHash h) = application1 "PubKeyHash" h

instance ShowBS V1.ScriptHash where
  {-# INLINEABLE showBS #-}
  showBS (V1.ScriptHash h) = application1 "ScriptHash" h

instance ShowBS V1.Credential where
  {-# INLINEABLE showBS #-}
  showBS (V1.ScriptCredential hash) = application1 "ScriptCredential" hash
  showBS (V1.PubKeyCredential pkh) = application1 "PubKeyCredential" pkh

instance ShowBS V1.StakingCredential where
  {-# INLINEABLE showBS #-}
  showBS (V1.StakingHash cred) = application1 "StakingCredential" cred
  showBS (V1.StakingPtr i j k) = application3 "StakingPointer" i j k

instance ShowBS V1.DatumHash where
  {-# INLINEABLE showBS #-}
  showBS (V1.DatumHash h) = application1 "DatumHash" h

instance ShowBS V1.Datum where
  {-# INLINEABLE showBS #-}
  showBS (V1.Datum d) = application1 "Datum" d

instance ShowBS V1.Redeemer where
  {-# INLINEABLE showBS #-}
  showBS (V1.Redeemer builtinData) = application1 "Redeemer" builtinData

instance ShowBS V1.POSIXTime where
  {-# INLINEABLE showBS #-}
  showBS (V1.POSIXTime t) = application1 "POSIXTime" t

instance (ShowBS a) => ShowBS (V1.Extended a) where
  {-# INLINEABLE showBS #-}
  showBS V1.NegInf = "NegInf"
  showBS V1.PosInf = "PosInf"
  showBS (V1.Finite x) = application1 "Finite" x

instance (ShowBS a) => ShowBS (V1.LowerBound a) where
  {-# INLINEABLE showBS #-}
  showBS (V1.LowerBound x closure) = application2 "LowerBound" x closure

instance (ShowBS a) => ShowBS (V1.UpperBound a) where
  {-# INLINEABLE showBS #-}
  showBS (V1.UpperBound x closure) = application2 "UpperBound" x closure

instance (ShowBS a) => ShowBS (V1.Interval a) where
  {-# INLINEABLE showBS #-}
  showBS (V1.Interval lb ub) = application2 "Interval" lb ub
