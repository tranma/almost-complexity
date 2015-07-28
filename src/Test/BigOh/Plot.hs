{-# LANGUAGE ViewPatterns #-}
-- * Plot simple y(x) functions in ASCII
--
module Test.BigOh.Plot where

import Control.Monad
import Control.Arrow
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed
import qualified Data.Array.ST as AS
import qualified Data.Array.MArray as AM
import Data.Ord
import Data.Function
import qualified Data.List as L


type Range  = (Double, Double)
type Points = [(Double, Double)]

data Plot
  = Plot
  { plotWidth  :: Int
  , plotHeight :: Int
  , plotPoints :: [(Double, Double)] }
  deriving Show

plotToArray :: Plot -> UArray (Int, Int) Char
plotToArray (Plot width height points)
  = AS.runSTUArray
  $ do let maxX   = L.maximum $ fmap fst points
           maxY   = L.maximum $ fmap snd points
           scaleX = maxX / fromIntegral width
           scaleY = maxY / fromIntegral height
           scaled = fmap ((/scaleX) *** (/scaleY)) points
       a <- AM.newArray ((0,0), (width + 1, height + 1)) ' '
       forM_ scaled
         $ \(x,y) -> AM.writeArray a (round x, height - round y) 'x'
       return a

printArray :: UArray (Int, Int) Char -> IO ()
printArray a
  = do let (minB, maxB) = bounds a
           row i        = [ a ! (x, i) | x <- [fst minB .. snd maxB] ]
           thing        = fmap row [snd minB .. snd maxB]
       mapM_ putStrLn thing
