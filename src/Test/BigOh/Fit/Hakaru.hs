{-# LANGUAGE MultiWayIf #-}
module Test.BigOh.Fit.Hakaru
  ( -- * Complexity defs
    Coefficient
  , Order(..)
  , Fit(..)
  , knownOrders
  , exponential
  , constant
  , linear
  , quadratic
  , cubic
  , nlogn
  , poly1d

  -- * Fitting curves
  , bestFit
  , fit
  ) where

import           Control.Arrow
import           Control.Monad
import           Data.Dynamic
import           Data.List
import           Data.Monoid
import           Data.Ord
import qualified Data.Vector                  as V
import           Language.Hakaru.Distribution
import           Language.Hakaru.Metropolis
import           Language.Hakaru.Types
import qualified Statistics.Sample            as S

import           Test.BigOh.Fit.Base


data Fit
  = Fit { xVals :: [Double]
        , yVals :: [Double]
        , burnN :: Int
        , takeN :: Int
        }

-- | Given a complexity order and some data,
--   determine if the order best describes the data.
fit :: Order -> [Order] -> [Double] -> [Double] -> IO Bool
fit order orders xs ys
  = go (50 :: Int) burnStart takeStart
  where
   burnStart = 10000
   takeStart = 1000
   go try b t
    = do ranked    <- bestFit' orders (Fit xs ys b t)
         putStrLn ("ranked: " <> show (fmap (first name) ranked))
         let order' = fst $ head ranked
         if | name order == name order' -> return True
            | try        == 0           -> return False
            | otherwise                 -> go (try - 1) (b * 2) (t * 2)

-- | Find the complexity order that best fits the data.
--
bestFit :: [Order] -> Fit ->  IO Order
bestFit orders f
  = fst . head <$> bestFit' orders f

bestFit' :: [Order] -> Fit ->  IO [(Order, Double)]
bestFit' orders f@(Fit xs ys _ _)
  = do fits   <- mapM (`curveFit` f) orders
       let fs  = fmap (<$> xs) fits
           rs  = fmap (rSquared ys) fs
           rs' = zip orders rs
           rs''= sortBy (comparing snd) rs'
       return rs''

-- | Given a complexity order and some data, generate a curve of
--   that order that fit the data.
--
curveFit :: Order -> Fit -> IO (Double -> Double)
curveFit thing (Fit xs ys dropn taken)
 = do l <- mcmc (measureForOrder thing xs ys)
                (map (Just . toDyn . Lebesgue) ys)
      let means = expectations $ take taken $ drop dropn l
      return $ mkCurve thing means

-- | Create a sampler for a class of curves with some x values.
--   e.g. sample @y = a*x^2 + b*x +c@
--
measureForOrder :: Order -> [Double] -> [Double] -> Measure [Double]
measureForOrder order xs ys
  = measureForOrder' 0 (maximum $ fmap abs ys) (sd ys) order xs

measureForOrder' :: Double -> Double -> Double -> Order -> [Double] -> Measure [Double]
measureForOrder' mean range sdev (Order _ n func) xs
  = do w <- replicateM n $ unconditioned (normal mean range)
       y <- mapM (conditioned . withinNormal w) xs
       return w
  where
   withinNormal w x
     = normal (func w x) sdev

-- | Given a bunch of possible coefficient sets, return the
--   expected value of each coeffient.
--   e.g. for @a*x^2 + b^x + c@,some possible coffients might be:
--        @[[a=0,b=1,c=4], [a=3,b=4,c=2]]@, @expectations@ returns
--        the expected values for @a, b, c@.
--
expectations :: [[Double]] -> [Double]
expectations l = map (S.mean . V.fromList) (transpose l)

--------------------------------------------------------------------------------

type Coefficient = Double

-- | A complexity order, e.g. exponential, quadratic.
data Order
  = Order
    { name      :: String
    , numCoeffs :: Int
    , mkCurve   :: [Coefficient] -> Double -> Double
    }

knownOrders :: [Order]
knownOrders = [exponential, constant, linear, quadratic, cubic, nlogn]

exponential :: Order
exponential
  = Order "exp" 4 $ \[a, b, c, d] x -> a * (2 ** (b * x + c)) + d

constant :: Order
constant
  = Order "constant" 1 poly1d

linear :: Order
linear
  = Order "linear" 2 poly1d

quadratic :: Order
quadratic
  = Order "quadratic" 3 poly1d

cubic :: Order
cubic
  = Order "cubic" 4 poly1d

nlogn :: Order
nlogn
  = Order "nlogn" 2
  $ \[a, b] n -> a * n * log  n + b

poly1d :: [Double] -> Double -> Double
poly1d weights a = poly weights a 1
   where
     poly [] _ _ = 0
     poly (w:ws) x acc = w*acc + (poly ws x acc*x)

square :: Num a => a -> a
square x = x * x

rSquared
  :: [Double] -- ^ data set y1..yn
  -> [Double] -- ^ model f1..fn
  -> Double   -- ^ r squared
rSquared ys fs
 = let yBar  = sum ys / fromIntegral (length ys)
       ssTot = sum (fmap (square . subtract yBar) ys)
       ssRes = sum (fmap square (zipWith (-) ys fs))
   in  (ssTot - ssRes) / ssTot
