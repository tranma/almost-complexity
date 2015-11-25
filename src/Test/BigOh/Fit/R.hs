{-# LANGUAGE GADTs       #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.BigOh.Fit.R where

import           Data.Ix
import           Data.Monoid
import           Data.Vector.SEXP
import qualified Foreign.R           as R
import           Language.R.HExp
import           Language.R.Instance as R
import           Language.R.QQ
import           Test.BigOh.Fit.Base
import           Test.BigOh.Plot


data Order
  = Constant
  | LogN
  | Linear
  | NLogN
  | Quadratic
  | Cubic
  | Quartic
  deriving (Show, Eq, Ord, Bounded, Enum, Ix)

knownOrders :: [Order]
knownOrders = [minBound..maxBound]

lm :: Order -> [Point] -> IO Double
lm order points
 = let (xs, ys) = unzip points
   in R.runRegion
        $ do it <- lm' order xs ys
             R.unSomeSEXP it
               $ \it'
               -> case hexp it' of
                    Real v -> headM v
                    Int  v -> fromIntegral <$> headM v
                    _      -> error "R result isn't a number"

lm' :: Order -> [Double] -> [Double] -> R s (R.SomeSEXP s)
lm' order x y
  = do [r| x = x_hs |]
       [r| y = y_hs |]
       case order of
         Constant    -> [r|summary(lm(y ~ 1))$adj.r.squared|]
         LogN        -> [r|summary(lm(y ~ I(log(x))))$adj.r.squared|]
         NLogN       -> [r|summary(lm(y ~ I(x * log(x))))$adj.r.squared|]
         Linear      -> [r|summary(lm(y ~ I(x)))$adj.r.squared|]
         Quadratic   -> [r|summary(lm(y ~ I(x^2) + I(x)))$adj.r.squared|]
         Cubic       -> [r|summary(lm(y ~ I(x^3) + I(x^2) + I(x)))$adj.r.squared|]
         Quartic     -> [r|summary(lm(y ~ I(x^4) + I(x^3) + I(x^2) + I(x)))$adj.r.squared|]

pretty :: Order -> String
pretty Constant    = "1"
pretty LogN        = "log n"
pretty Linear      = "n"
pretty NLogN       = "n log n"
pretty Quadratic   = "n" <> superscript 2
pretty Cubic       = "n" <> superscript 3
pretty Quartic     = "n" <> superscript 4
