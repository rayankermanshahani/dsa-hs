module Main (main) where

import Criterion.Main (bench, bgroup, defaultMain)
import qualified DataStructures.ArrayBench as ArrayBench

-- import qualified DataStructures.BinarySearchBench as BinarySearchBench
-- add imports as more modules are implemented

main :: IO ()
main =
  defaultMain
    [ bgroup
        "Data Structures"
        [ ArrayBench.benchmarks
        -- add more data structure benchmarks here
        ],
      bgroup
        "Algorithms"
        []
    ]
