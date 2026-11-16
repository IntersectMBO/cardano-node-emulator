module Main (main) where

import JSONSpec qualified
import ShowBSSpec qualified
import Test.Tasty (defaultMain, testGroup)
import V3.PurposeDispatcher qualified as PurposeDispatcher
import V4.PurposeDispatcher qualified as PurposeDispatcherV4

main :: IO ()
main =
  defaultMain $
    testGroup
      "all tests"
      [ PurposeDispatcher.tests,
        PurposeDispatcherV4.tests,
        JSONSpec.tests,
        ShowBSSpec.tests
      ]
