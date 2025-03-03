module DataStructures.ArrayBench (benchmarks) where

import Control.DeepSeq (NFData)
import Criterion.Main (Benchmark, bench, bgroup, nf)
import DataStructures.Array (example)

-- benchmark group for Array module
benchmarks :: Benchmark
benchmarks =
  bgroup
    "Array"
    [ bench "example function" $ nf example 42
    -- add more benchmarks here
    ]
