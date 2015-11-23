module Test.BigOh (module X, complexityWhnf) where

import           Test.BigOh.Benchmark as X
import           Test.BigOh.Generate  as X
import           Test.BigOh.Plot      as X
import qualified Test.BigOh.Fit.Naive  as Naive

import System.Console.ANSI
import System.Console.Ansigraph
import Test.QuickCheck
import Control.Arrow

complexityWhnf :: (Int -> Gen s) -> (s -> x) -> Int -> (Int, Int) -> (Int -> Bool) -> IO Bool
complexityWhnf gen func numSamples range predicate = do
       x    <- generate $ genWhnf numSamples range gen func
       reps <- runInputs benchmarkConfig x
       let points = first fromIntegral <$> getTimes reps
       let epsilon = minimum $ getStdDevs reps
       mapM_ (posgraph . fmap (fromIntegral :: Int -> Double))
             (plotToGraphs $ Plot 32 64 points)
       case Naive.polyOrder epsilon points of
         Just o | predicate o -> passed $ "PASSED: Seems to be O(n" ++ superscript o ++ ")"
                | otherwise   -> failed $ "FAILED: Seems to be O(n" ++ superscript o ++ ")"
         Nothing              -> inconclusive "INCONCLUSIVE: Ran out of points! Maybe it's not polynomial?"

  where
    withColor c r x = do
      setSGR [SetColor Foreground Vivid c]
      putStrLn x
      setSGR [Reset]
      return r
      
    passed = withColor Green True
    failed = withColor Red False
    inconclusive = withColor Yellow False

    superscript = map go . show
      where
        go '0' = '⁰'
        go '1' = '¹'
        go '2' = '²'
        go '3' = '³'
        go '4' = '⁴'
        go '5' = '⁵'
        go '6' = '⁶'
        go '7' = '⁷'
        go '8' = '⁸'
        go '9' = '⁹'
