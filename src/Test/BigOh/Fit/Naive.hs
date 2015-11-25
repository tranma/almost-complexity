module Test.BigOh.Fit.Naive
  ( Order
  , fit
  , polyOrder
  , deriv
  ) where

import           Test.BigOh.Fit.Base

type Order = Int -- ahahaha


-- | Estimate the polynomial order for some points, given
--   a margin of error.
--
polyOrder :: Double -> [Point] -> Maybe Order
polyOrder epsilon points@(_:_:_:_)
  | isConstant points
  = Just 0
  | otherwise
  = fmap succ $ polyOrder epsilon $ deriv points
  where
   isConstant derivs
    = sd (fmap snd derivs) < epsilon
polyOrder _ _
  = Nothing

deriv :: [Point] -> [Point]
deriv points
  = let gs = zipWith gradient points (drop 1 points)
    in  zip (fmap fst points) gs

gradient :: Point -> Point -> Double
gradient (x1, y1) (x2, y2)
   = (y2 - y1) / (x2 - x1)

-- doesn't work very well, just use polyOrder
fit :: Order -> [Point] -> Bool
fit order points = go (100 :: Int) startEpsilon
  where
    startEpsilon
      = 2.1e-6 -- 0.1 * sd (fmap snd points)
    go 0 _
      = False
    go n e
      = case polyOrder e points of
          -- haven't found an order
          Nothing
            -> go (n - 1) (e * 2 / 3)
          -- found an order
          Just order'
            -- but it's not the right one, it's less
            | order' < order
            -> go (n - 1) (e * 2)
            -- but it's not the right one, it's more
            | order' > order
            -> go (n - 1) (e * 2 / 3)
            -- bingo
            | otherwise
            -> True
