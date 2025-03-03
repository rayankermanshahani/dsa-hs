module DataStructures.ArraySpec (tests) where

import qualified Data.List as List
import DataStructures.Array
  ( Array,
    delete,
    empty,
    fromList,
    insert,
    lookup,
    size,
    toList,
    update,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import Test.Tasty.QuickCheck
  ( Arbitrary,
    Gen,
    Property,
    arbitrary,
    counterexample,
    forAll,
    property,
    suchThat,
    testProperty,
    (.&&.),
    (===),
  )
import Prelude hiding (lookup)

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
