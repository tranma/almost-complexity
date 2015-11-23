import           Data.Monoid
import           System.Console.Ansigraph
import           Test.QuickCheck

import           Test.BigOh
import           Test.BigOh.Fit.Hakaru
import           Test.BigOh.Fit.Naive


main
  = do x      <- generate $ genWhnf 20 (1,20) return fib
       points <- runtimes x
       h      <- bestFit knownOrders (fmap fst points) (fmap snd points)
       let s   = polyOrder 0.5 points
       mapM_ (posgraph . fmap (fromIntegral :: Int -> Double))
             (plotToGraphs $ Plot 32 32 points)
       putStrLn $ "hakaru says: " <> name h
       putStrLn $ "shitty says poly order: " <> show s

fib :: Int -> Int
fib m | m < 0     = error "negative!"
      | otherwise = go m
  where go 0 = 0
        go 1 = 1
        go n = go (n-1) + go (n-2)

