module Main (main) where

import Test.Tasty (defaultMain)
import V3.PurposeDispatcher qualified as PurposeDispatcher

main :: IO ()
main = defaultMain PurposeDispatcher.tests
