module Test.BigOh where

import Control.Applicative
import Test.BigOh.Generate
import Test.BigOh.Benchmark
import Test.BigOh.Plot
import Control.Arrow
import Criterion.Main
import Test.QuickCheck

foo
  = do x <- generate $ genWhnf 10 (1,20) id fib
       y <- map (first fromIntegral) . getTimes <$> runInputs defaultConfig x
       let p = Plot 32 32 y
       printArray $ plotToArray p

fib m | m < 0     = error "negative!"
      | otherwise = go m
  where go 0 = 0
        go 1 = 1
        go n = go (n-1) + go (n-2)

