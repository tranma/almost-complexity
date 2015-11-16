{-# OPTIONS_GHC -fwarn-missing-signatures #-}
module Test.BigOh.Fit where

import           Control.Monad
import           Data.Dynamic
import           Data.List
import           Data.Ord
import qualified Data.Vector                  as V
import           Language.Hakaru.Distribution
import           Language.Hakaru.Metropolis
import           Language.Hakaru.Types
import qualified Statistics.Sample            as S

expectations :: [[Double]] -> [Double]
expectations l = map (S.mean . V.fromList) (transpose l)

type Coefficient = Double

data Order
  = Order
    { name      :: String
    , numCoeffs :: Int
    , mkCurve   :: [Coefficient] -> Double -> Double }

poly1d :: [Double] -> Double -> Double
poly1d weights a = poly weights a 1
   where
     poly [] _ _ = 0
     poly (w:ws) x acc = w*acc + (poly ws x acc*x)

exponential
  = Order "exp" 4 $ \[a, b, c, d] x -> a * (2 ** (b * x + c)) + d

constant
  = Order "constant" 1 poly1d

linear
  = Order "linear" 2 poly1d

quadratic
  = Order "quadratic" 3 poly1d

cubic
  = Order "cubic" 4 poly1d

measureForOrder :: Order -> [Double] -> Measure [Double]
measureForOrder (Order _ n f) x = do
    w <- replicateM n $ unconditioned (normal 0 2)
    mapM_ (conditioned . flip normal 1 . f w) x
    return w

fit
  :: Order                          -- ^ a type of curve
  -> [Double]                       -- ^ x's
  -> [Double]                       -- ^ y's
  -> IO (Double -> Double)          -- ^ a curve
fit thing x y
 = do l <- mcmc (measureForOrder thing x) (map (Just . toDyn . Lebesgue) y)
      let means = expectations $ take 1000 $ drop 5000 l
      return $ mkCurve thing means

bestFit
  :: [Order]
  -> [Double]                       -- ^ x's
  -> [Double]                       -- ^ y's
  ->  IO Order
bestFit orders xs ys
  = do fits   <- mapM (\o -> fit o xs ys) orders
       let fs  = fmap (<$> xs) fits
           rs  = fmap (rSquared ys) fs
           rs' = zip orders rs
           r   = minimumBy (comparing snd) rs'
       return (fst r)

--------------------------------------------------------------------------------

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
   in  1 - (ssRes / ssTot)
