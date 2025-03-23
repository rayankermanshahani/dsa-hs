{-# LANGUAGE ScopedTypeVariables #-}

module Algorithms.BinarySearchSpec (tests) where

import Algorithms.BinarySearch
  ( binarySearch,
    binarySearchBy,
    binarySearchList,
    binarySearchListBy,
    lowerBound,
    upperBound,
  )
import Data.List (sort, sortBy)
import qualified DataStructures.Array as Array
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck
  ( choose,
    forAll,
    property,
    testProperty,
    (===),
    (==>),
  )

-- test tree for BinarySearch module
tests :: TestTree
tests =
  testGroup
    "BinarySearch"
    [ testGroup
        "Unit Tests"
        [ testCase "Binary search on empty array returns Nothing" $
            binarySearch (5 :: Int) Array.empty @?= Nothing,
          testCase "Binary search finds element in middle of array" $ do
            let arr = Array.fromList [10, 20, 30, 40, 50 :: Int]
            binarySearch 30 arr @?= Just 2,
          testCase "Binary search finds element at beginning of array" $ do
            let arr = Array.fromList [10, 20, 30, 40, 50 :: Int]
            binarySearch 10 arr @?= Just 0,
          testCase "Binary search finds element at end of array" $ do
            let arr = Array.fromList [10, 20, 30, 40, 50 :: Int]
            binarySearch 50 arr @?= Just 4,
          testCase "Binary search returns Nothing for element not in array" $ do
            let arr = Array.fromList [10, 20, 30, 40, 50 :: Int]
            binarySearch 25 arr @?= Nothing,
          testCase "Binary search works with custom comparison function" $ do
            let arr = Array.fromList [(1, "one"), (2, "two"), (3, "three") :: (Int, String)]
                cmp (a, _) (b, _) = compare a b
            binarySearchBy cmp (2, "anything") arr @?= Just 1,
          testCase "Binary search on list works correctly" $ do
            let lst = [10, 20, 30, 40, 50 :: Int]
            binarySearchList 30 lst @?= Just 2,
          testCase "Binary search on list works with custom comparison function" $ do
            let lst = [(1, "apple"), (3, "banana"), (5, "cherry")]
                cmp (a :: Int, _) (b :: Int, _) = compare a b
            binarySearchListBy cmp (3, "anything") lst @?= Just 1,
          testCase "Binary search on list with custom comparison function works with duplicate values" $ do
            let lst = [10, 20, 20, 20, 30, 40, 50 :: Int]
            -- It should find one of the 20s, but we can't predict which one
            case binarySearchListBy compare 20 lst of
              Just i -> i >= 1 && i <= 3 @?= True
              Nothing -> False @?= True,
          testCase "Binary search works with duplicate values" $ do
            let arr = Array.fromList [10, 20, 20, 20, 30, 40, 50 :: Int]
            -- it should find one of the 20s, but we can't predict which one
            case binarySearch 20 arr of
              Just i -> i >= 1 && i <= 3 @?= True
              Nothing -> False @?= True,
          testCase "lowerBound returns correct index for element in array" $ do
            let arr = Array.fromList [10, 20, 30, 40, 50 :: Int]
            lowerBound 20 arr @?= 1,
          testCase "lowerBound returns correct index for element not in array" $ do
            let arr = Array.fromList [10, 20, 30, 40, 50 :: Int]
            lowerBound 25 arr @?= 2,
          testCase "lowerBound works for elements less than all in array" $ do
            let arr = Array.fromList [10, 20, 30, 40, 50 :: Int]
            lowerBound 5 arr @?= 0,
          testCase "lowerBound works for elements greater than all in array" $ do
            let arr = Array.fromList [10, 20, 30, 40, 50 :: Int]
            lowerBound 60 arr @?= 5,
          testCase "lowerBound correctly handles duplicates" $ do
            let arr = Array.fromList [10, 20, 20, 20, 30, 40, 50 :: Int]
            lowerBound 20 arr @?= 1,
          testCase "upperBound returns correct index for element in array" $ do
            let arr = Array.fromList [10, 20, 30, 40, 50 :: Int]
            upperBound 20 arr @?= 2,
          testCase "upperBound returns correct index for element not in array" $ do
            let arr = Array.fromList [10, 20, 30, 40, 50 :: Int]
            upperBound 25 arr @?= 2,
          testCase "upperBound works for elements less than all in array" $ do
            let arr = Array.fromList [10, 20, 30, 40, 50 :: Int]
            upperBound 5 arr @?= 0,
          testCase "upperBound works for elements greater than all in array" $ do
            let arr = Array.fromList [10, 20, 30, 40, 50 :: Int]
            upperBound 60 arr @?= 5,
          testCase "upperBound correctly handles duplicates" $ do
            let arr = Array.fromList [10, 20, 20, 20, 30, 40, 50 :: Int]
            upperBound 20 arr @?= 4

            -- add more unit tests here as needed
        ],
      testGroup
        "Property Tests"
        [ testProperty "Binary search finds elements at correct index" $
            \(xs :: [Int]) ->
              not (null xs) ==>
                let sorted = sort xs
                    arr = Array.fromList sorted
                 in forAll (choose (0, length xs - 1)) $ \i ->
                      let x = sorted !! i
                       in case binarySearch x arr of
                            Just found -> Array.lookup found arr === Just x
                            Nothing -> property False,
          testProperty "Binary search properly deals with duplicates" $
            \(xs :: [Int], x :: Int) ->
              let sorted = sort (replicate 3 x ++ xs)
                  arr = Array.fromList sorted
               in case binarySearch x arr of
                    Just _ -> property True
                    Nothing -> property False,
          testProperty "binarySearchListBy finds element at correct index" $
            \(xs :: [Int]) ->
              not (null xs) ==>
                let sorted = sort xs
                 in forAll (choose (0, length xs - 1)) $ \i ->
                      let x = sorted !! i
                       in case binarySearchListBy compare x sorted of
                            Just found -> sorted !! found == x
                            Nothing -> False,
          testProperty "binarySearchListBy with custom comparator works correctly" $
            \(xs :: [(Int, Int)]) ->
              not (null xs) ==>
                let sortedByFirst = sortBy (\(a, _) (b, _) -> compare a b) xs
                    cmp (a, _) (b, _) = compare a b
                 in forAll (choose (0, length xs - 1)) $ \i ->
                      let (x, _) = sortedByFirst !! i
                       in case binarySearchListBy cmp (x, 0) sortedByFirst of
                            Just found -> let (foundX, _) = sortedByFirst !! found in foundX == x
                            Nothing -> False,
          testProperty "binarySearchListBy with reversed ordering works correctly" $
            \(xs :: [Int]) ->
              not (null xs) ==>
                let sortedDesc = sortBy (flip compare) xs
                    cmp a b = compare b a -- Reverse comparison
                 in forAll (choose (0, length xs - 1)) $ \i ->
                      let x = sortedDesc !! i
                       in case binarySearchListBy cmp x sortedDesc of
                            Just found -> sortedDesc !! found == x
                            Nothing -> False,
          testProperty "lowerBound is always <= upperBound" $
            \(xs :: [Int], x :: Int) ->
              let sorted = sort xs
                  arr = Array.fromList sorted
               in lowerBound x arr <= upperBound x arr,
          testProperty "lowerBound finds first element >= target" $
            \(xs :: [Int], x :: Int) ->
              not (null xs) ==>
                let sorted = sort xs
                    arr = Array.fromList sorted
                    lb = lowerBound x arr
                 in lb <= length sorted ==>
                      ( lb == length sorted || case Array.lookup lb arr of
                          Just val -> val >= x
                          Nothing -> False
                      )
                        && ( lb == 0
                               || case Array.lookup (lb - 1) arr of
                                 Just val -> val < x
                                 Nothing -> False
                           ),
          testProperty "upperBound finds first element > target" $
            \(xs :: [Int], x :: Int) ->
              not (null xs) ==>
                let sorted = sort xs
                    arr = Array.fromList sorted
                    ub = upperBound x arr
                 in ub <= length sorted ==>
                      ( ub == length sorted || case Array.lookup ub arr of
                          Just val -> val > x
                          Nothing -> False
                      )
                        && ( ub == 0 || case Array.lookup (ub - 1) arr of
                               Just val -> val <= x
                               Nothing -> False
                           )
                           -- add more property tests here as needed
        ]
    ]
