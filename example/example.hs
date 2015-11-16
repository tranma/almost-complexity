import           Control.Applicative
import           Control.Arrow
import           Criterion.Main
import           Test.BigOh
import           Test.QuickCheck
import System.Console.Ansigraph
import Data.List



main
  = do x <- generate $ genWhnf 20 (1,20) (\n -> take n <$> infiniteList) --
                                         (sort :: [Int] -> [Int]) --fib
       y <- map (first fromIntegral) . getTimes <$> runInputs defaultConfig x
       let p = Plot 32 32 y
       -- printArray $ plotToArray p
       -- mapM (posgraph . fmap (fromIntegral :: Int -> Double)) $ plotToGraphs p
       bleh <- bestFit [exponential, quadratic, linear] (fmap fst y) (fmap snd y)
       print $ name bleh
       print y


fib m | m < 0     = error "negative!"
      | otherwise = go m
  where go 0 = 0
        go 1 = 1
        go n = go (n-1) + go (n-2)

