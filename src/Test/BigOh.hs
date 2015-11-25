module Test.BigOh
  ( Settings(..)
  , module X
  , naiveWhnf
  , rlmWhnf
  , mcmcWhnf
  , naive
  , rlm
  , mcmc
  , genRunWhnf
  , defaultsForListInput
  , defaultsForIntInput
  , getPoints
  , rVoodoo
  ) where

import           Control.Arrow
import           Criterion.Types
import           Data.List
import           Data.Monoid
import           Data.Ord
import           Language.R.Instance   as R
import           Test.BigOh.Benchmark  as X
import           Test.BigOh.Fit.Base
import qualified Test.BigOh.Fit.Hakaru as H
import qualified Test.BigOh.Fit.Naive  as Naive
import qualified Test.BigOh.Fit.R      as R
import           Test.BigOh.Generate   as X
import           Test.BigOh.Plot       as X
import           Test.QuickCheck
import qualified Criterion.Types as C

import Debug.Trace


data Settings s
  = Settings
  { generator   :: Int -> Gen s
  , numSamples  :: Int
  , sampleRange :: (Int, Int)
  , benchConf   :: C.Config
  }

genRunWhnf :: Settings s -> (s -> x) -> IO [(Input s, Report)]
genRunWhnf (Settings gen n r conf) func
  = do x <- generate $ genWhnf n r gen func
       runInputs conf x

getPoints :: [(Input a, Report)] -> [Point]
getPoints xs
  = first fromIntegral <$> getTimes xs

goodnessOfRSquared :: Double -> Double
goodnessOfRSquared x
 | x /= x    = 1 / 0 -- fucking NaN
 | otherwise = abs (1 - x)

defaultsForListInput :: Arbitrary a => Settings [a]
defaultsForListInput
  = Settings (\n -> take n <$> infiniteList) 10 (50, 2000) defaultBenchmarkConfig

defaultsForIntInput :: Settings Int
defaultsForIntInput
  = Settings return 10 (5,30) defaultBenchmarkConfig

rVoodoo :: IO a -> IO a
rVoodoo = R.withEmbeddedR R.defaultConfig

--------------------------------------------------------------------------------

naiveWhnf :: Settings s -> (s -> x) -> (Naive.Order -> Bool) -> IO Bool
naiveWhnf ss f predi = genRunWhnf ss f >>= naive predi

rlmWhnf :: Settings s -> (s -> x) -> (R.Order -> Bool) -> IO Bool
rlmWhnf   ss f predi = genRunWhnf ss f >>= rlm predi

mcmcWhnf :: Settings s -> (s -> x) -> (H.Order -> Bool) -> IO Bool
mcmcWhnf  ss f predi = genRunWhnf ss f >>= mcmc predi

naive :: (Naive.Order -> Bool) -> [(Input a, Report)] -> IO Bool
naive predicate reps
  = do let points  = getPoints reps
       let epsilon = minimum $ getStdDevs reps

       case Naive.polyOrder epsilon points of
         Just o
           | predicate o -> passed $ "PASSED: Seems to be O(n" ++ superscript o ++ ")"
           | otherwise   -> failed $ "FAILED: Seems to be O(n" ++ superscript o ++ ")"
         Nothing
           -> inconclusive "INCONCLUSIVE: Ran out of points! Maybe it's not polynomial?"

rlm :: (R.Order -> Bool) -> [(Input a, Report)] -> IO Bool
rlm predi reps
  = do let points = getPoints reps
       lms       <- mapM (flip R.lm points) R.knownOrders
       let lms'   = zip R.knownOrders lms
           best   = trace ("rsquares: " <> show lms') $ minimumBy (comparing (goodnessOfRSquared . snd)) lms'
           b      = predi (fst best)

       if b
       then passed $ "PASSED: Seems to be O(" <> R.pretty (fst best) <> ")"
       else failed $ "FAILED: Seems to be O(" <> R.pretty (fst best) <> ")"

       return b

mcmc :: (H.Order -> Bool) -> [(Input a, Report)] -> IO Bool
mcmc predi reps
  = do let points   = getPoints reps
           (xs, ys) = unzip points
       H.fit predi H.knownOrders xs ys
