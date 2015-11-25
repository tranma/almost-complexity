import           Data.Monoid
import           Test.BigOh
import qualified Test.BigOh.Fit.R as R

main
  = rVoodoo
  $ do header "Fibonacci"
       go (> 2) (== R.Exp) fib
  where
    go i o f
      = do x <- genRunWhnf (defaultsForIntInput { numSamples = 10, sampleRange = (1,20) }) f
           graphPoints (getPoints x)
           putStrLn $ "xs=" <> show (fst $ unzip $ getPoints x)
           putStrLn $ "ys=" <> show (snd $ unzip $ getPoints x)
           header "Naive:"
           naive i x
           header  "R:"
           rlm   o x

fib :: Int -> Int
fib m | m < 0     = error "negative!"
      | otherwise = go m
  where go 0 = 0
        go 1 = 1
        go n = go (n-1) + go (n-2)

