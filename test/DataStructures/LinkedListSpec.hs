{-# LANGUAGE ScopedTypeVariables #-}

module DataStructures.LinkedListSpec (tests) where

import DataStructures.LinkedList
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck
  ( choose,
    forAll,
    testProperty,
    (===),
    (==>),
  )

-- test tree for LinkedList module
tests :: TestTree
tests =
  testGroup
    "LinkedList"
    [ testGroup
        "Singly Linked List"
        [ testGroup
            "Unit Tests"
            [ testCase "Empty singly linked list has size 0" $
                singlySize singlyEmpty @?= 0,
              testCase "singlyFromList creates list with correct size" $
                singlySize (singlyFromList [1, 2, 3 :: Int]) @?= 3,
              testCase "singlyLookup returns correct elements" $ do
                let lst = singlyFromList [10, 20, 30, 40 :: Int]
                singlyLookup 0 lst @?= Just 10
                singlyLookup 1 lst @?= Just 10
                singlyLookup 2 lst @?= Just 10
                singlyLookup 3 lst @?= Just 10
                singlyLookup 4 lst @?= Nothing
                singlyLookup (-1) lst @?= Nothing,
              testCase "singlyHead returns the first element" $ do
                let lst = singlyFromList [1, 2, 3 :: Int]
                singlyHead lst @?= Just 1
                singlyHead (singlyEmpty :: SinglyLinkedList Int) @?= Nothing,
              testCase "singlyTail returns all elements except the first" $ do
                let lst = singlyFromList [1, 2, 3 :: Int]
                fmap singlyToList (singlyTail lst) @?= Just [2, 3]
                singlyTail (singlyEmpty :: SinglyLinkedList Int) @?= Nothing,
              testCase "singlyInsert adds element at specific index" $ do
                let lst = singlyFromList [1, 2, 4 :: Int]
                    lst' = singlyInsert 2 3 lst
                singlyToList lst' @?= [1, 2, 3, 4],
              testCase "singlyInsert handles edge cases" $ do
                let lst = singlyFromList [1, 2, 3 :: Int]
                    lst1 = singlyInsert (-1) 0 lst
                    lst2 = singlyInsert 5 4 lst
                singlyToList lst1 @?= [1, 2, 3]
                singlyToList lst2 @?= [1, 2, 3],
              testCase "singlyDelete removes element at specific index" $ do
                let lst = singlyFromList [1, 2, 3, 4 :: Int]
                    lst' = singlyDelete 1 lst
                singlyToList lst' @?= [1, 3, 4],
              testCase "singlyDelete handles edge cases" $ do
                let lst = singlyFromList [1, 2, 3 :: Int]
                    lst1 = singlyDelete (-1) lst
                    lst2 = singlyDelete 5 lst
                singlyToList lst1 @?= [1, 2, 3]
                singlyToList lst2 @?= [1, 2, 3],
              testCase "singlyUpdate changes element at specific index" $ do
                let lst = singlyFromList [1, 2, 3 :: Int]
                    lst' = singlyUpdate 1 69 lst
                singlyToList lst' @?= [1, 69, 3],
              testCase "singleUpdate handles edge cases" $ do
                let lst = singlyFromList [1, 2, 3 :: Int]
                    lst1 = singlyUpdate (-1) 69 lst
                    lst2 = singlyUpdate 5 69 lst
                singlyToList lst1 @?= [1, 2, 3]
                singlyToList lst2 @?= [1, 2, 3],
              testCase "singlyAppend adds element to the end" $ do
                let lst = singlyFromList [1, 2, 3 :: Int]
                    lst' = singlyAppend 4 lst
                singlyToList lst' @?= [1, 2, 3, 4],
              testCase "singlyPrepend adds element to the beginning" $ do
                let lst = singlyFromList [1, 2, 3 :: Int]
                    lst' = singlyPrepend 0 lst
                singlyToList lst' @?= [0, 1, 2, 3],
              testCase "singlyToList roundtrip" $ do
                let xs = [1, 2, 3, 4 :: Int]
                    lst = singlyFromList xs
                singlyToList lst @?= xs
            ],
          testGroup
            "Property Tests"
            [ testProperty "singlyFromList/singlyToList roundtrip" $
                \(xs :: [Int]) -> singlyToList (singlyFromList xs) === xs,
              testProperty "singlySize matches list length" $
                \(xs :: [Int]) -> singlySize (singlyFromList xs) === length xs,
              testProperty "singlyLookup returns correct elements" $
                \(xs :: [Int]) ->
                  not (null xs) ==>
                    forAll (choose (0, length xs - 1)) $ \i ->
                      singlyLookup i (singlyFromList xs) === Just (xs !! i),
              testProperty "singlyInsert followed by singlyLookup returns inserted element" $
                \(xs :: [Int]) (x :: Int) ->
                  not (null xs) ==>
                    forAll (choose (0, length xs - 1)) $ \i ->
                      singlyLookup i (singlyInsert i x (singlyFromList xs)) === Just x,
              testProperty "singlyDelete reduces size by 1" $
                \(xs :: [Int]) ->
                  not (null xs) ==>
                    forAll (choose (0, length xs - 1)) $ \i ->
                      singlySize (singlyDelete i (singlyFromList xs)) === singlySize (singlyFromList xs) - 1,
              testProperty "singlyUpdate preserves size" $
                \(xs :: [Int]) (x :: Int) ->
                  not (null xs) ==>
                    forAll (choose (0, length xs - 1)) $ \i ->
                      singlySize (singlyUpdate i x (singlyFromList xs)) === singlySize (singlyFromList xs),
              testProperty "singlyAppend increases size by 1" $
                \(xs :: [Int]) (x :: Int) ->
                  singlySize (singlyAppend x (singlyFromList xs)) === singlySize (singlyFromList xs) + 1,
              testProperty "singlyPrepend increases size by 1" $
                \(xs :: [Int]) (x :: Int) ->
                  singlySize (singlyPrepend x (singlyFromList xs)) === singlySize (singlyFromList xs) + 1
            ]
        ],
      testGroup
        "Doubly Linked List"
        [ testGroup
            "Unit Tests"
            [ testCase "Empty doubly linked list has size 0" $
                doublySize doublyEmpty @?= 0,
              testCase "doublyFromList creates list with correct size" $
                doublySize (doublyFromList [1, 2, 3 :: Int]) @?= 3,
              testCase "doublyLookup returns correct elements" $ do
                let list = doublyFromList [10, 20, 30, 40 :: Int]
                doublyLookup 0 list @?= Just 10
                doublyLookup 2 list @?= Just 30
                doublyLookup 3 list @?= Just 40
                doublyLookup 4 list @?= Nothing
                doublyLookup (-1) list @?= Nothing,
              testCase "doublyHead returns the first element" $ do
                let list = doublyFromList [1, 2, 3 :: Int]
                doublyHead list @?= Just 1
                doublyHead (doublyEmpty :: DoublyLinkedList Int) @?= Nothing,
              testCase "doublyTail returns all elements except the first" $ do
                let list = doublyFromList [1, 2, 3 :: Int]
                doublyToList (doublyTail list) @?= [2, 3]
                doublyTail (doublyEmpty :: DoublyLinkedList Int) @?= doublyEmpty,
              testCase "doublyLast returns the last element" $ do
                let list = doublyFromList [1, 2, 3 :: Int]
                doublyLast list @?= Just 3
                doublyLast (doublyEmpty :: DoublyLinkedList Int) @?= Nothing,
              testCase "doublyInit returns all elements except the last" $ do
                let list = doublyFromList [1, 2, 3 :: Int]
                doublyToList (doublyInit list) @?= [1, 2]
                doublyInit (doublyEmpty :: DoublyLinkedList Int) @?= doublyEmpty,
              testCase "doublyInsert adds element at specific index" $ do
                let list = doublyFromList [1, 2, 4 :: Int]
                    list' = doublyInsert 2 3 list
                doublyToList list' @?= [1, 2, 3, 4],
              testCase "doublyDelete removes element at specific index" $ do
                let list = doublyFromList [1, 2, 3, 4 :: Int]
                    list' = doublyDelete 1 list
                doublyToList list' @?= [1, 3, 4],
              testCase "doublyUpdate changes element at specific index" $ do
                let list = doublyFromList [1, 2, 3 :: Int]
                    list' = doublyUpdate 1 99 list
                doublyToList list' @?= [1, 99, 3],
              testCase "doublyAppend adds element to the end" $ do
                let list = doublyFromList [1, 2, 3 :: Int]
                    list' = doublyAppend 4 list
                doublyToList list' @?= [1, 2, 3, 4],
              testCase "doublyPrepend adds element to the beginning" $ do
                let list = doublyFromList [2, 3, 4 :: Int]
                    list' = doublyPrepend 1 list
                doublyToList list' @?= [1, 2, 3, 4],
              testCase "doublyForward moves cursor forward" $ do
                let list = doublyFromList [1, 2, 3 :: Int]
                    list' = doublyForward list
                doublyCurrent list' @?= Just 2,
              testCase "doublyBackward moves cursor backward" $ do
                let list = doublyFromList [1, 2, 3 :: Int]
                    list' = doublyForward list
                    list'' = doublyBackward list'
                doublyCurrent list'' @?= Just 1,
              testCase "doublyAtStart detects if cursor is at start" $ do
                let list = doublyFromList [1, 2, 3 :: Int]
                doublyAtStart list @?= True
                doublyAtStart (doublyForward list) @?= False,
              testCase "doublyAtEnd detects if cursor is at end" $ do
                let list = doublyFromList [1, 2, 3 :: Int]
                doublyAtEnd list @?= False
                doublyAtEnd (doublyForward (doublyForward list)) @?= True,
              testCase "doublyToList roundtrip" $ do
                let xs = [1, 2, 3, 4 :: Int]
                    list = doublyFromList xs
                doublyToList list @?= xs
            ],
          testGroup
            "Property Tests"
            [ testProperty "doublyFromList/doublyToList roundtrip" $
                \(xs :: [Int]) -> doublyToList (doublyFromList xs) === xs,
              testProperty "doublySize matches list length" $
                \(xs :: [Int]) -> doublySize (doublyFromList xs) === length xs,
              testProperty "doublyLookup returns correct elements" $
                \(xs :: [Int]) ->
                  not (null xs) ==>
                    forAll (choose (0, length xs - 1)) $ \i ->
                      doublyLookup i (doublyFromList xs) === Just (xs !! i),
              testProperty "doublyInsert followed by doublyLookup returns inserted element" $
                \(xs :: [Int]) (x :: Int) ->
                  not (null xs) ==>
                    forAll (choose (0, length xs - 1)) $ \i ->
                      doublyLookup i (doublyInsert i x (doublyFromList xs)) === Just x,
              testProperty "doublyDelete reduces size by 1" $
                \(xs :: [Int]) ->
                  not (null xs) ==>
                    forAll (choose (0, length xs - 1)) $ \i ->
                      doublySize (doublyDelete i (doublyFromList xs)) === doublySize (doublyFromList xs) - 1,
              testProperty "doublyUpdate preserves size" $
                \(xs :: [Int]) (x :: Int) ->
                  not (null xs) ==>
                    forAll (choose (0, length xs - 1)) $ \i ->
                      doublySize (doublyUpdate i x (doublyFromList xs)) === doublySize (doublyFromList xs),
              testProperty "doublyAppend increases size by 1" $
                \(xs :: [Int]) (x :: Int) ->
                  doublySize (doublyAppend x (doublyFromList xs)) === doublySize (doublyFromList xs) + 1,
              testProperty "doublyPrepend increases size by 1" $
                \(xs :: [Int]) (x :: Int) ->
                  doublySize (doublyPrepend x (doublyFromList xs)) === doublySize (doublyFromList xs) + 1,
              testProperty "doublyForward/doublyBackward are inverses" $
                \(xs :: [Int]) ->
                  not (null xs) && length xs > 1 ==>
                    doublyToList (doublyBackward (doublyForward (doublyFromList xs))) === doublyToList (doublyFromList xs)
            ]
        ]
    ]
