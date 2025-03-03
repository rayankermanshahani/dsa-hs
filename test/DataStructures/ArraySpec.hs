module DataStructures.ArraySpec (tests) where

import DataStructures.Array (example)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)

-- test tree for Array module
tests :: TestTree
tests =
  testGroup
    "Array"
    [ testGroup
        "Unit Tests"
        [ testCase "Example function returns correct value" $
            example 42 @?= 42
            -- add more unit tests here
        ],
      testGroup
        "Property Tests"
        [ testProperty "Example function preserves input" $
            \x -> example x == x
            -- add more property tests here
        ]
        --  add more test groups here as needed
    ]
