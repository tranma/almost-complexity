module Test.BigOh.Fit.Base where

import Data.List

type Point = (Double, Double)

variance :: [Double] -> Double
variance xs@(_:_:_)
 = let (n,_,m2) = foldl' go (0,0,0) xs
   in   m2 / (n - 1)
 where
  go (n,m,m2) x
   = let n'    = n + 1
         delta = x - m
         m'    = m + delta / n'
         m2'   = m2 + delta * (x - m')
     in  (n',m',m2')
variance _ = 0 / 0

sd :: [Double] -> Double
sd = sqrt . variance

mean :: [Double] -> Double
mean xs = sum xs / fromIntegral (length xs)
