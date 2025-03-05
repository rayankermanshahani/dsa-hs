{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DataStructures.ArraySpec (tests) where

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
    choose,
    counterexample,
    forAll,
    property,
    suchThat,
    testProperty,
    (.&&.),
    (===),
    (==>),
  )
import Prelude hiding (lookup)

-- test tree for Array module
tests :: TestTree
tests =
  testGroup
    "Array"
    [ testGroup
        "Unit Tests"
        [ testCase "Empty array has size 0" $
            size empty @?= 0,
          testCase "fromList creates array with correct size" $
            size (fromList [1, 2, 3 :: Int]) @?= 3,
          testCase "lookup returns the correct elements" $ do
            let arr = fromList [10, 20, 30, 40 :: Int]
            lookup 0 arr @?= Just 10
            lookup 1 arr @?= Just 20
            lookup 2 arr @?= Just 30
            lookup 3 arr @?= Just 40
            lookup (-1) arr @?= Nothing,
          testCase "insert adds elements at specific index" $ do
            let arr = fromList [1, 2, 4 :: Int]
            let arr' = insert 2 3 arr
            toList arr' @?= [1, 2, 3, 4],
          testCase "insert ignores out of bounds index" $ do
            let arr = fromList [1, 2, 3 :: Int]
            let arr' = insert 5 99 arr
            toList arr' @?= [1, 2, 3],
          testCase "delete removes elements at specific index" $ do
            let arr = fromList [1, 2, 3, 4 :: Int]
            let arr' = delete 1 arr
            toList arr' @?= [1, 3, 4],
          testCase "delete ignores out of bounds index" $ do
            let arr = fromList [1, 2, 3 :: Int]
            let arr' = delete 5 arr
            toList arr' @?= [1, 2, 3],
          testCase "update changes element at specific index" $ do
            let arr = fromList [1, 2, 3 :: Int]
            let arr' = update 1 69 arr
            toList arr' @?= [1, 69, 3],
          testCase "update ignores out of bounds index" $ do
            let arr = fromList [1, 2, 3 :: Int]
            let arr' = update 5 69 arr
            toList arr' @?= [1, 2, 3],
          testCase "toList roundtrip" $ do
            let xs = [1, 2, 3, 4 :: Int]
            let arr = fromList xs
            toList arr @?= xs

            -- add more unit tests here
        ],
      testGroup
        "Property Tests"
        [ testProperty "fromList/toList roundtrip" $
            \(xs :: [Int]) -> toList (fromList xs) === xs,
          testProperty "size matches list length" $
            \(xs :: [Int]) -> size (fromList xs) === length xs,
          testProperty "lookup returns correct elements" $
            \(xs :: [Int]) ->
              not (null xs) ==>
                forAll
                  (choose (0, length xs - 1))
                  (\i -> lookup i (fromList xs) === Just (xs !! i)),
          testProperty "insert followed by lookup returns inserted element" $
            \(xs :: [Int]) (x :: Int) ->
              not (null xs) ==>
                forAll
                  (choose (0, length xs - 1))
                  (\i -> lookup i (insert i x (fromList xs)) === Just x),
          testProperty "delete reduces size by 1" $
            \(xs :: [Int]) ->
              not (null xs) ==>
                forAll
                  (choose (0, length xs - 1))
                  (\i -> size (delete i (fromList xs)) === size (fromList xs) - 1),
          testProperty "update preserves size" $
            \(xs :: [Int]) (x :: Int) ->
              not (null xs) ==>
                forAll
                  (choose (0, length xs - 1))
                  (\i -> size (update i x (fromList xs)) === size (fromList xs))
                  -- add more property tests here
        ]
        --  add more test groups here as needed
    ]

-- | helper function to generate indices within a specific range
-- choose :: (Int, Int) -> Gen Int
-- choose (min', max') = arbitrary `suchThat` \n -> n >= min' && n <= max'
