-- * This module defines arbitrary-based benchmark generators.
--
module Test.BigOh.Generate
  ( -- * Benchmark generators
    Input(..)
  , genWhnf
  , genNf
  , genInputs
  , genIndependentInputs
  ) where

import           Control.Applicative
import           Prelude
import           Control.DeepSeq
import           Criterion.Main
import qualified Data.List              as L
import           Test.QuickCheck
import           Control.Monad

data Input a
  = Input
  { input :: a
  , inputSize :: Int
  } deriving (Show)


-- | Given a function `f :: a -> b`, generate inputs of type `a`,
--   then apply the generated inputs to `f` and evaluate to
--   weak head-normal form.
--
genWhnf
  :: Int                      -- ^ max number of inputs
  -> (Int, Int)               -- ^ range of input size
  -> (Int -> Gen a)           -- ^ given a size, how to generate an input
  -> (a -> b)                 -- ^ function to evaluate
  -> Gen [(Benchmarkable, Input a)] -- ^ the inputs and their benchmarks
genWhnf n range fromSize func
  = do xs <- genInputs n range fromSize
       return $ zipWith ((,) . whnf func . input) xs xs

-- | Given a function `f :: a -> b`, generate inputs of type `a`,
--   then apply the generated inputs to `f` and evaluate to
--   head-normal form.
--
genNf
  :: (NFData b)
  => Int                            -- ^ max number of inputs
  -> (Int, Int)                     -- ^ range of input size
  -> (Int -> Gen a)                 -- ^ given a size, how to generate an input
  -> (a -> b)                       -- ^ function to evaluate
  -> Gen [(Benchmarkable, Input a)] -- ^ the inputs and their benchmarks
genNf n range fromSize func
  = do xs <- genInputs n range fromSize
       return $ zipWith ((,) . nf func . input) xs xs

genInputs
  ::  Int            -- ^ max number of inputs
  -> (Int, Int)      -- ^ range of input size
  -> (Int -> Gen a)  -- ^ given a size, generate an input
  -> Gen [Input a]
genInputs n (x,y) f
  = let step = max 1 $ (y - x) `div` n
        sizes = takeWhile (< y) $ iterate (+ step) x
    in  zipWithM (\a b -> Input <$> f a <*> pure b) sizes sizes

genIndependentInputs :: Int -> (Int, Int) -> (Int -> Gen a) -> Gen [Input a]
genIndependentInputs n range f
  = do xs <- take n . map getPositive <$> infiniteList
       let sorted = L.nub $ L.sort xs
           high   = L.last sorted
           low    = L.head sorted
           sizes  = map (linmap (low, high) range) sorted
       zipWithM (\a b -> Input <$> f a <*> pure b) sizes sizes

linmap :: Integral a => (a, a) -> (a, a) -> a -> a
linmap (x1, y1) (x2, y2) v
  = x2 + (v - x1) * (y2 - x2) `div` (y1 - x1)
