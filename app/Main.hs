module Main (main) where

import qualified DataStructures.Array as Array
import System.Environment (getArgs)
import Text.Show.Pretty (pPrint)

-- import other modules as they are implemented

-- | main entry point
main :: IO ()
main = do
  args <- getArgs
  case args of
    ["demo", "array"] -> demoArray
    _ -> showUsage

-- | show usage information
showUsage :: IO ()
showUsage = do
  putStrLn "dsa-hs: Data Structures and Algorithms in Haskell"
  putStrLn "Usage:"
  putStrLn "  cabal run dsa-hs -- demo array            Run Array demo"
  putStrLn ""
  putStrLn "Testing:"
  putStrLn "  cabal test                                Run all tests"

demoArray :: IO ()
demoArray = do
  putStrLn "Array Data Structure Demo"
  putStrLn "========================="

  let arr = Array.fromList [1 .. 5] :: Array.Array Int
  putStrLn $ "Initial array: " ++ show arr

  putStrLn $ "Size: " ++ show (Array.size arr)
  putStrLn $ "Element at index 2: " ++ show (Array.lookup 2 arr)

  let arr2 = Array.insert 2 99 arr
  putStrLn $ "After inserting 99 at index 2: " ++ show arr2

  let arr3 = Array.delete 1 arr2
  putStrLn $ "After deleting index 1: " ++ show arr3

  let arr4 = Array.update 0 69 arr3
  putStrLn $ "After updating index 0 to 69: " ++ show arr4

  putStrLn $ "As list: " ++ show (Array.toList arr4)
