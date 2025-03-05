module DataStructures.LinkedList
  ( -- * singly linked list (sll) type
    SinglyLinkedList,

    -- * sll construction
    singlyEmpty,
    singlyFromList,

    -- * sll conversion
    singlyToList,

    -- * sll query
    singlyLookup,
    singlySize,
    singlyHead,
    singlyTail,

    -- * ssl modification
    singlyInsert,
    singlyDelete,
    singlyUpdate,
    singlyAppend,
    singlyPrepend,

    -- * doubly linked list (dll) type
    DoublyLinkedList,

    -- * dll construction
    doublyEmpty,
    doublyFromList,

    -- * dll conversion
    doublyToList,

    -- * dll query
    doublyLookup,
    doublySize,
    doublyHead,
    doublyTail,
    doublyLast,
    doublyInit,

    -- * dll modification
    doublyInsert,
    doublyDelete,
    doublyUpdate,
    doublyAppend,
    doublyPrepend,

    -- * dll traversal
    doublyForward,
    doublyBackard,
    doublyAtStart,
    doublyAtEnd,
    doublyCurrent,

    -- * example (for testing)
    example,
  )
where

import qualified Data.List as List
import Prelude hiding (head, init, last, tail)

-- | a singly linked list
data SinglyLinkedList a = SNil | SCons a (SinglyLinkedList a)
  deriving (Show, Eq)

-- | create an empty singly linked list
singlyEmpty :: SinglyLinkedList a
singlyEmpty = SNil

-- | create a singly linked list from a list
singlyFromList :: [a] -> SinglyLinkedList a
singlyFromList = foldr SCons SNil

-- | convert a singly linked list to a list
singlyToList :: SinglyLinkedList a -> [a]
singlyToList SNil = []
singlyToList (SCons x xs) = x : singlyToList xs

-- | look up an element by index in a singly linked list
singlyLookup :: Int -> SinglyLinkedList a -> Maybe a
singlyLookup _ SNil = Nothing
singlyLookup 0 (SCons x _) = Just x
singlyLookup i (SCons _ xs)
  | i > 0 = singlyLookup (i - 1) xs
  | otherwise = Nothing

-- | get size of singly linked list
singlySize :: SinglyLinkedList a -> Int
singlySize SNil = 0
singlySize (SCons _ xs) = 1 + singlySize xs

-- | get head of a singly linked list
singlyHead :: SinglyLinkedList a -> Maybe a
singlyHead SNil = Nothing
singlyHead (SCons x _) = Just x

-- | get tail of a singly linked list
singlyTail :: SinglyLinkedList a -> Maybe a
singlyTail SNil = Nothing
singlyTail (SCons _ xs) = Just xs

-- | insert element at a specified index in a singly linked list
singlyInsert :: Int -> a -> SinglyLinkedList a -> SinglyLinkedList a
singlyInsert 0 x xs = SCons x xs -- insert at head
singlyInsert _ x SNil = SCons x SNil -- insert at end
singlyInsert i x (SCons y ys)
  | i > 0 = SCons y (singlyInsert (i - 1) x ys)
  | otherwise = SCons x (SCons y ys) -- return unchanged list for negative indices

-- | delete element at a specific index from a singly linked list
singlyDelete :: Int -> SinglyLinkedList a -> SinglyLinkedList a
singlyDelete _ SNil = SNil
singlyDelete 0 (SCons _ xs) = xs
singlyDelete i (SCons x xs)
  | i > 0 = SCons x (singlyDelete (i - 1) xs)
  | otherwise = SCons x xs

-- | update element at a specific index in a singly linked list
singlyUpdate :: Int -> a -> SinglyLinkedList a -> SinglyLinkedList a
singlyUpdate _ _ SNil = SNil
singlyUpdate 0 x (SCons _ ys) = SCons x ys
singlyUpdate i x (SCons y ys)
  | i > 0 = SCons y (singlyUpdate (i - 1) x ys)
  | otherwise = SCons y ys

-- | append an element to a singly linked list
singlyAppend :: a -> SinglyLinkedList a
singlyAppend x SNil = SCons x SNil
singlyAppend x (SCons y ys) = SCons y (singlyAppend x ys)

-- | prepend an element to a singly linked list
singlyPrepend :: a -> SinglyLinkedList a
singlyPrepend = SCons

-- | a doubly linked list represented as a zipper
data DoublyLinkedList a = DoublyLinkedList
  { -- | elements to the left of the focus (in reverse order)
    leftElements :: [a],
    -- | element at the focus (if any)
    focusElement :: Maybe a,
    -- | elements to the right of the focus
    rightElements :: [a]
  }
  deriving (Show, Eq)

-- * dll construction

doublyEmpty :: DoublyLinkedList a
doublyEmpty =
  DoublyLinkedList
    { leftElements = [],
      focusElement = Nothing,
      rightElements = []
    }

doublyFromList :: [a] -> DoublyLinkedList a
doublyFromList [] = doublyEmpty
doublyFromList (x : xs) =
  DoublyLinkedList
    { leftElements = [],
      focusElement = Just x,
      rightElements = xs
    }

-- | convert a doubly linked list to a list
doublyToList :: DoublyLinkedList a -> [a]
doublyToList
  DoublyLinkedList
    { leftElements = left,
      focusElement = focus,
      rightElements = right
    } =
    reverse left ++ maybeToList focus ++ right

-- | look up an element by index in a doubly likned list
doublyLookup :: Int -> DoublyLinkedList a -> Maybe a
doublyLookup i dll
  | i < 0 = Nothing
  | otherwise = case drop i (doublyToList dll) of
      (x : _) -> Just x
      [] -> Nothing

-- | get size of a doubly linked list
doublySize :: DoublyLinkedList a -> Int
doublySize = length . doublyToList

-- | get head of doubly linked list
doublyHead :: DoublyLinkedList a -> Maybe a
doublyHead dll = listToMaybe . doublyToLi

-- doublyTail,
-- doublyLast,
-- doublyInit,
--
-- -- * dll modification
-- doublyInsert,
-- doublyDelete,
-- doublyUpdate,
-- doublyAppend,
-- doublyPrepend,
--
-- -- * dll traversal
-- doublyForward,
-- doublyBackard,
-- doublyAtStart,
-- doublyAtEnd,
-- doublyCurrent,

-- | example function for testing
example :: a -> a

example id
