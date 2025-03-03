module DataStructures.Array
  ( -- * types
    Array,

    -- * construction
    fromList,
    empty,

    -- * query
    lookup,
    size,

    -- * modification
    insert,
    delete,
    update,

    -- * conversion
    toList,

    -- * example (for testing)
    example,
  )
where

import qualified Data.Array as A
import Data.Maybe (fromMaybe)
import Prelude hiding (lookup)

-- | a simple array implementation using Data.Array
data Array a = Array
  { arrayData :: A.Array Int a,
    arraySize :: Int
  }
  deriving (Show, Eq)

-- | create an array from a list
fromList :: [a] -> Array a
fromList xs =
  Array
    { arrayData = A.listArray (0, length xs - 1) xs,
      arraySize = length xs
    }

-- | create an empty array
empty :: Array a
empty =
  Array
    { arrayData = A.listArray (0, -1) [],
      arraySize = 0
    }

-- | look up an element by index
lookup :: Int -> Array a -> Maybe a
lookup i arr
  | i < 0 || i >= arraySize arr = Nothing
  | otherwise = Just $ arrayData arr A.! i

-- | get size of an array
size :: Array a -> Int
size = arraySize

-- | insert element at specific index
insert :: Int -> a -> Array a -> Array a
insert i x arr
  -- nothing happens if index is out of bounds
  | i < 0 || i >= arraySize arr = arr
  | otherwise =
      Array
        { arrayData =
            A.array (0, arraySize arr) $
              [ ( j,
                  if j < i
                    then arrayData arr A.! j
                    else
                      if j == i
                        then x
                        else arrayData arr A.! (j - 1)
                )
                | j <- [0 .. arraySize arr]
              ],
          arraySize = arraySize arr + 1
        }

-- | delete element at specific index
delete :: Int -> Array a -> Array a
delete i arr
  -- nothing happens if index is out of bounds
  | i < 0 || i >= arraySize arr = arr
  | otherwise =
      Array
        { arrayData =
            A.array (0, arraySize arr - 2) $
              [ ( j,
                  if j < i
                    then arrayData arr A.! j
                    else arrayData arr A.! (j + 1)
                )
                | j <- [0 .. arraySize arr - 2]
              ],
          arraySize = arraySize arr - 1
        }

-- | update element at specific index
update :: Int -> a -> Array a -> Array a
update i x arr
  | i < 0 || i >= arraySize arr = arr
  | otherwise = arr {arrayData = arrayData arr A.// [(i, x)]}

-- | convert an array to a list
toList :: Array a -> [a]
toList arr = [arrayData arr A.! i | i <- [0 .. arraySize arr - 1]]

-- | example function for testing
example :: a -> a
example = id
