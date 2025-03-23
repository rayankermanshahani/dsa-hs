module Main (main) where

import System.Environment (getArgs)

-- import other modules as they are implemented

-- | main entry point
main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> demo
    _ -> showUsage

-- | show usage information
showUsage :: IO ()
showUsage = do
  putStrLn "dsa-hs: Data Structures and Algorithms in Haskell"
  putStrLn "Usage:"
  putStrLn "  cabal run dsa-hs                          Run Array demo"
  putStrLn ""
  putStrLn "Testing:"
  putStrLn "  cabal test                                Run all tests"

demo :: IO ()
demo = do
  putStrLn "Running Demo:"
  putStrLn "==============="
