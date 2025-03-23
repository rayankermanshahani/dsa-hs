module Algorithms.BinarySearch
  ( -- * binary search on arrays
    binarySearch,
    binarySearchBy,

    -- * binary search on lists
    binarySearchList,
    binarySearchListBy,

    -- * variants
    lowerBound,
    upperBound,

    -- * example (for testing)
    example,
  )
where

import qualified DataStructures.Array as Array
import Prelude hiding (lookup)

-- | binary search on a sorted array
binarySearch :: (Ord a) => a -> Array.Array a -> Maybe Int
binarySearch = binarySearchBy compare

-- | binary search with custom comparison function on a sorted array
binarySearchBy :: (a -> a -> Ordering) -> a -> Array.Array a -> Maybe Int
binarySearchBy _ _ arr | Array.size arr == 0 = Nothing
binarySearchBy cmp x arr = go 0 (Array.size arr - 1)
  where
    go lo hi
      | lo > hi = Nothing
      | otherwise =
          let mid = lo + (hi - lo) `div` 2 -- avoid integer overflow
           in case Array.lookup mid arr of
                Nothing -> Nothing -- should never happen if indices are valid
                Just midVal ->
                  case cmp x midVal of
                    EQ -> Just mid
                    LT -> go lo (mid - 1)
                    GT -> go (mid + 1) hi

-- | binary search on a sorted list
binarySearchList :: (Ord a) => a -> [a] -> Maybe Int
binarySearchList = binarySearchListBy compare

-- | binary search with custom comparison function on a sorted list
binarySearchListBy :: (a -> a -> Ordering) -> a -> [a] -> Maybe Int
binarySearchListBy _ _ [] = Nothing
binarySearchListBy cmp x xs = go 0 (length xs - 1)
  where
    go lo hi
      | lo > hi = Nothing
      | otherwise =
          let mid = lo + (hi - lo) `div` 2
              midVal = xs !! mid
           in case cmp x midVal of
                EQ -> Just mid
                LT -> go lo (mid - 1)
                GT -> go (mid + 1) hi

-- | find index of first element greater than or equal to target in a sorted array
lowerBound :: (Ord a) => a -> Array.Array a -> Int
lowerBound x arr = go 0 (Array.size arr - 1)
  where
    go lo hi
      | lo >= hi = lo
      | otherwise =
          let mid = lo + (hi - lo) `div` 2
           in case Array.lookup mid arr of
                Nothing -> lo
                Just midVal ->
                  if midVal < x
                    then go (mid + 1) hi
                    else go lo mid

-- | find index of first element strictly greater than target in a sorted array
upperBound :: (Ord a) => a -> Array.Array a -> Int
upperBound x arr = go 0 (Array.size arr - 1)
  where
    go lo hi
      | lo >= hi = lo
      | otherwise =
          let mid = lo + (hi - lo) `div` 2
           in case Array.lookup mid arr of
                Nothing -> lo
                Just midVal ->
                  if midVal <= x
                    then go (mid + 1) hi
                    else go lo mid

-- | example function for testing
example :: a -> a
example = id
