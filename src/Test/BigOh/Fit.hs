module Test.BigOh.Fit where

import           Control.Monad
import qualified Data.Vector                  as V
import           Language.Hakaru.Distribution
import           Language.Hakaru.Metropolis
import           Language.Hakaru.Types
import qualified Statistics.Matrix            as S
import qualified Statistics.Sample            as S



poly1d weights a = poly weights a 1
   where
     poly [] _ _ = 0
     poly (w:ws) x acc = w*acc + (poly ws x acc*x)

cubic :: [Double] -> Measure [Double]
cubic x = do
    w <- replicateM 4 $ unconditioned (normal 0 2)
    mapM_ (conditioned . flip normal 1 . poly1d w) x
    return w
