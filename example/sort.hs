import           Criterion.Types
import qualified Data.Discrimination as D
import           Data.Ix
import           Data.List

import           Test.BigOh
import qualified Test.BigOh.Fit.R    as R


main
  = rVoodoo
  $ do header "Selection sort"
       go (== 2) (== R.Quadratic) selection
       header  "Quick sort"
       go (inRange (1,2)) (inRange (R.Linear,R.Quadratic)) (sort :: [Int] -> [Int])
       header  "Discrimination (radix) sort"
       go (== 1) (== R.Linear) (D.sort :: [Int] -> [Int])
  where
   go i o f
     = do x <- genRunWhnf (defaultsForListInput
                             { benchConf = defaultBenchmarkConfig { timeLimit = 2 }})
                          f
          graphPoints $ getPoints x
          header "Naive:"
          naive i x
          header  "R:"
          rlm   o x

selection :: [Int] -> [Int]
selection [] = []
selection xs = let x = maximum xs in selection (delete x xs) ++ [x]
