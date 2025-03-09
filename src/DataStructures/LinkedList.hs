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
    doublyBackward,
    doublyAtStart,
    doublyAtEnd,
    doublyCurrent,

    -- * example (for testing)
    example,
  )
where

import Data.Maybe (listToMaybe, maybeToList)

-- import Prelude hiding (head, init, last, tail)

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
singlyTail :: SinglyLinkedList a -> Maybe (SinglyLinkedList a)
singlyTail SNil = Nothing
singlyTail (SCons _ xs) = Just xs

-- | insert element at a specified index in a singly linked list
singlyInsert :: Int -> a -> SinglyLinkedList a -> SinglyLinkedList a
singlyInsert 0 x xs = SCons x xs -- insert at head
singlyInsert _ x SNil = SCons x SNil -- insert into empty list
singlyInsert i x (SCons y ys)
  | i > 0 && i <= singlySize ys + 1 = SCons y (singlyInsert (i - 1) x ys)
  | i <= 0 = SCons y ys
  | otherwise = SCons y ys -- return unchanged liust if index is negative

-- | delete element at a specific index from a singly linked list
singlyDelete :: Int -> SinglyLinkedList a -> SinglyLinkedList a
singlyDelete _ SNil = SNil
singlyDelete 0 (SCons _ xs) = xs
singlyDelete i (SCons x xs)
  | i > 0 && i <= singlySize xs + 1 = SCons x (singlyDelete (i - 1) xs)
  | otherwise = SCons x xs -- return unchanged list if index is negative

-- | update element at a specific index in a singly linked list
singlyUpdate :: Int -> a -> SinglyLinkedList a -> SinglyLinkedList a
singlyUpdate _ _ SNil = SNil
singlyUpdate 0 x (SCons _ ys) = SCons x ys
singlyUpdate i x (SCons y ys)
  | i > 0 && i <= singlySize ys + 1 = SCons y (singlyUpdate (i - 1) x ys)
  | otherwise = SCons y ys -- return unchanged list if index is negative

-- | append an element to a singly linked list
singlyAppend :: a -> SinglyLinkedList a -> SinglyLinkedList a
singlyAppend x SNil = SCons x SNil
singlyAppend x (SCons y ys) = SCons y (singlyAppend x ys)

-- | prepend an element to a singly linked list
singlyPrepend :: a -> SinglyLinkedList a -> SinglyLinkedList a
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

-- | get head of a doubly linked list
doublyHead :: DoublyLinkedList a -> Maybe a
doublyHead = listToMaybe . doublyToList

-- | get last element of a doubly linked list
doublyLast :: DoublyLinkedList a -> Maybe a
doublyLast dll =
  case reverse (doublyToList dll) of
    (x : _) -> Just x
    [] -> Nothing

-- | get tail of a doubly linked list
doublyTail :: DoublyLinkedList a -> DoublyLinkedList a
doublyTail dll =
  case doublyToList dll of
    (_ : xs) -> doublyFromList xs
    [] -> doublyEmpty

-- | get all but the last element of a doubly linked list
doublyInit :: DoublyLinkedList a -> DoublyLinkedList a
doublyInit dll =
  case doublyToList dll of
    xs
      | null xs -> doublyEmpty
      | otherwise -> doublyFromList $ init xs

-- | insert an element at specific index in a doubly linked list
doublyInsert :: Int -> a -> DoublyLinkedList a -> DoublyLinkedList a
doublyInsert i x dll
  | i < 0 = dll
  | otherwise =
      let lst = doublyToList dll
          (before, after) = splitAt i lst
       in doublyFromList $ before ++ x : after

-- | delete an element at specific index from a doubly linked list
doublyDelete :: Int -> DoublyLinkedList a -> DoublyLinkedList a
doublyDelete i dll
  | i < 0 || i >= doublySize dll = dll
  | otherwise =
      let lst = doublyToList dll
       in case splitAt i lst of
            (before, _ : after) -> doublyFromList $ before ++ after
            _ -> dll -- return unchanged dll if index is out of bounds

-- | update an element at specific index from a doubly linked list
doublyUpdate :: Int -> a -> DoublyLinkedList a -> DoublyLinkedList a
doublyUpdate i x dll
  | i < 0 || i >= doublySize dll = dll
  | otherwise =
      let lst = doublyToList dll
       in case splitAt i lst of
            (before, _ : after) -> doublyFromList $ before ++ x : after
            _ -> dll -- return unchanged dll if index is out of bounds

-- | append an element to the end of a doubly linked list
doublyAppend :: a -> DoublyLinkedList a -> DoublyLinkedList a
doublyAppend x dll =
  let lst = doublyToList dll
   in doublyFromList (lst ++ [x])

-- | prepend an element to the beginning of a doubly linked list
doublyPrepend :: a -> DoublyLinkedList a -> DoublyLinkedList a
doublyPrepend x dll =
  let lst = doublyToList dll
   in doublyFromList (x : lst)

-- | move focus forward in the doubly linked list
doublyForward :: DoublyLinkedList a -> DoublyLinkedList a
doublyForward
  dll@DoublyLinkedList -- list has no focus element and is empty to the right
    { leftElements = _,
      focusElement = Nothing,
      rightElements = []
    } = dll
doublyForward
  DoublyLinkedList -- list has a focus element but is empty to the right
    { leftElements = left,
      focusElement = focus,
      rightElements = []
    } =
    DoublyLinkedList
      { leftElements = maybe left (: left) focus,
        focusElement = Nothing,
        rightElements = []
      }
doublyForward
  DoublyLinkedList -- list has elements to the right
    { leftElements = left,
      focusElement = focus,
      rightElements = (r : rs)
    } =
    DoublyLinkedList
      { leftElements = maybe left (: left) focus,
        focusElement = Just r,
        rightElements = rs
      }

-- | move focus backward in the doubly linked list
doublyBackward :: DoublyLinkedList a -> DoublyLinkedList a
doublyBackward
  dll@DoublyLinkedList -- list is empty to the left (ie. at the start of the list)
    { leftElements = [],
      focusElement = _,
      rightElements = _
    } = dll
doublyBackward
  DoublyLinkedList -- list has elements to the left
    { leftElements = (l : ls),
      focusElement = focus,
      rightElements = right
    } =
    DoublyLinkedList
      { leftElements = ls,
        focusElement = Just l,
        rightElements = maybe right (: right) focus
      }

-- | check if focus is at the start of the doubly linked list
doublyAtStart :: DoublyLinkedList a -> Bool
doublyAtStart DoublyLinkedList {leftElements = []} = True
doublyAtStart _ = False

-- | check if focus is at the end of the doubly linked list
doublyAtEnd :: DoublyLinkedList a -> Bool
doublyAtEnd DoublyLinkedList {focusElement = Nothing, rightElements = []} = True
doublyAtEnd DoublyLinkedList {rightElements = []} = True
doublyAtEnd _ = False

-- | get the current element at the focus
doublyCurrent :: DoublyLinkedList a -> Maybe a
doublyCurrent = focusElement

-- | example function for testing
example :: a -> a
example = id
