{-# LANGUAGE FlexibleContexts #-}
module Test.BigOh.Plot where

import           Control.Arrow
import           Control.Monad
import           Control.Monad.ST
import qualified Data.Array.MArray        as AM
import           Data.Array.ST
import qualified Data.Array.ST            as AS
import           Data.Array.Unboxed
import qualified Data.List                as L
import           Data.Ord
import           Data.STRef
import           System.Console.ANSI
import           System.Console.Ansigraph

type Range  = (Double, Double)

data Plot
  = Plot
  { plotWidth  :: Int
  , plotHeight :: Int
  , plotPoints :: [(Double, Double)] }
  deriving Show

graphPoints :: [(Double, Double)] -> IO ()
graphPoints points
  | ps <- shiftUp points
  = mapM_ (posgraph . fmap (fromIntegral :: Int -> Double))
          (plotToGraphs $ Plot 32 64 ps)

shiftUp :: [(Double, Double)] -> [(Double, Double)]
shiftUp ps
  = let minY = snd $ L.minimumBy (comparing snd) ps
    in  if minY < 0
        then fmap (second (+ abs minY)) ps
        else ps

plotToGraphs :: Plot -> [[Int]]
plotToGraphs p@(Plot _ height _)
  = let ys = plotToYs p
        slice n = map (\v -> ((v - (n*8)) `max` 0) `min` 8) ys
     in reverse $ map slice [0..height `div` 8]

plotToYs :: Plot -> [Int]
plotToYs = grabYs . plotToArray

-- | Grab the highest y for each x
grabYs :: UArray (Int, Int) Char -> [Int]
grabYs a
  = let ((x0, y0), (xn, yn)) = bounds a
        ugh = reverse [y0..yn]
    in  flip fmap [x0..xn]
         $ \x -> case dropWhile (\y -> (a ! (x,y)) == ' ') ugh of
                   []     -> 0
                   (y':_) -> yn - y'

plotToArray :: Plot -> UArray (Int, Int) Char
plotToArray (Plot width height points)
  = AS.runSTUArray
  $ do let maxX   = L.maximum $ fmap fst points
           maxY   = L.maximum $ fmap snd points
           scaleX = maxX / fromIntegral width
           scaleY = maxY / fromIntegral height
           scaled = fmap ((/scaleX) *** (/scaleY)) points
       a <- AM.newArray ((0,0), (width, height)) ' '
       let scaled' = fmap go scaled
       let pairs = zip scaled' (drop 1 scaled')
       forM_ pairs $ uncurry (bresenham a 'x')
       return a
 where
  go (x,y) = (round x, height - round y)

printArray :: UArray (Int, Int) Char -> IO ()
printArray a
  = do let (minB, maxB) = bounds a
           row i        = [ a ! (x, i) | x <- [fst minB .. snd maxB] ]
           thing        = fmap row [snd minB .. snd maxB]
       mapM_ putStrLn thing

bresenham
  :: STUArray s (Int, Int) Char -> Char -> (Int, Int) -> (Int, Int) -> ST s ()
bresenham vec val (xa, ya) (xb, yb)
  = do yV     <- var y1
       errorV <- var $ deltax `div` 2
       forM_ [x1 .. x2] (\x -> do
         y <- get yV
         draw $ if steep then (y, x) else (x, y)
         mutate errorV $ subtract deltay
         err <- get errorV
         when (err < 0) (do
             mutate yV (+ ystep)
             mutate errorV (+ deltax)))
    where steep = abs (yb - ya) > abs (xb - xa)
          (xa', ya', xb', yb')
            = if steep
              then (ya, xa, yb, xb)
              else (xa, ya, xb, yb)
          (x1, y1, x2, y2)
            = if xa' > xb'
              then (xb', yb', xa', ya')
              else (xa', ya', xb', yb')
          deltax = x2 - x1
          deltay = abs $ y2 - y1
          ystep  = if y1 < y2 then 1 else -1
          var    = Data.STRef.newSTRef
          get    = Data.STRef.readSTRef
          mutate = Data.STRef.modifySTRef
          draw (x,y) = AM.writeArray vec (x,y) val

--------------------------------------------------------------------------------

withColor :: Color -> a -> String -> IO a
withColor c r x = do
  setSGR [SetColor Foreground Vivid c]
  putStrLn x
  setSGR [Reset]
  return r

passed, failed, inconclusive :: String -> IO Bool
passed       = withColor Green True
failed       = withColor Red False
inconclusive = withColor Yellow False

header = withColor Blue ()

superscript :: Int -> String
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
    go x   = x

