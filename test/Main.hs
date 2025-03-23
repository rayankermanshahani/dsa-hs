module Main (main) where

import qualified Algorithms.BinarySearchSpec as BinarySearchSpec
import qualified DataStructures.ArraySpec as ArraySpec
import qualified DataStructures.LinkedListSpec as LinkedListSpec
import Test.Tasty (TestTree, defaultMain, testGroup)

-- import qualified DataStructures.BinarySearchSpec as BinarySearchSpec
-- add imports as more modules are implemented

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "dsa-hs Test Suite"
    [ testGroup
        "Data Structures"
        [ ArraySpec.tests,
          LinkedListSpec.tests
          -- add more data structure tests here
        ],
      testGroup
        "Algorithms"
        [ BinarySearchSpec.tests -- add more algorithm tests here
        ]
    ]
