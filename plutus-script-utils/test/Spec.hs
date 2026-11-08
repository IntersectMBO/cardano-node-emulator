module Main (main) where

import JSONSpec qualified
import Test.Tasty (defaultMain, testGroup)
import V3.PurposeDispatcher qualified as PurposeDispatcher

main :: IO ()
main = defaultMain $ testGroup "all tests" [PurposeDispatcher.tests, JSONSpec.tests]
