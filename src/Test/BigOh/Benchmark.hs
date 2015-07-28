-- * This module defines how to run and extract time/space data
--   from benchmarks.
--
module Test.BigOh.Benchmark
  ( -- * Running benchmarks
    runInputs
  , getTimes
  ) where

import           Control.Applicative
import           Control.Monad.IO.Class
import           Criterion.Internal
import           Criterion.Main
import           Criterion.Measurement
import           Criterion.Monad
import           Criterion.Types
import           Statistics.Resampling.Bootstrap

import           Test.BigOh.Generate

runOne :: Config -> Benchmarkable -> IO Report
runOne cfg x
  = withConfig cfg
  $ do liftIO initializeTime
       runAndAnalyseOne 0 "" x

runInputs :: Config -> [(Benchmarkable, Input a)] -> IO [(Input a, Report)]
runInputs cfg xs
  = zip (map snd xs) <$> mapM (runOne cfg . fst) xs

getTimes :: [(Input a, Report)] -> [(Int, Double)]
getTimes = map go
  where
    go (i, report)
      = (inputSize i, estPoint $ anMean $ reportAnalysis report)
