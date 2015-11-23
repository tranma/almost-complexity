import           Data.List
import           Data.Monoid
import           System.Console.Ansigraph
import           Test.QuickCheck

import           Test.BigOh
import qualified Test.BigOh.Fit.Hakaru as Shit
import qualified Test.BigOh.Fit.Naive as Glorious

main
  = do --x      <- generate
       --        $ genWhnf 20 (1,20) (\n -> take n <$> infiniteList)
       --                            (sort :: [Int] -> [Int])
       let points = [(1.0,3.167386486797687e-8),(2.0,8.062574601155624e-8),(3.0,8.77738634654382e-8),(4.0,1.8236334861313467e-7),(5.0,2.5339146896927717e-7),(6.0,2.989270471462668e-7),(7.0,3.0723605217850713e-7),(8.0,3.8771295983283255e-7),(9.0,4.128735730849073e-7),(10.0,4.656314746044062e-7),(11.0,5.888679567094566e-7),(12.0,5.211566539907106e-7),(13.0,6.462910630622821e-7),(14.0,6.311127960837393e-7),(15.0,6.547534481453109e-7),(16.0,9.476858842859062e-7),(17.0,8.76069769938057e-7),(18.0,9.13435171974278e-7),(19.0,9.505668031036458e-7)]
       -- points <- runtimes x
       -- h      <- bestFit knownOrders (fmap fst points) (fmap snd points)
       -- h      <- fit quadratic knownOrders (fmap fst points) (fmap snd points)
       -- let s   = polyOrder 0.5 points
       let s = Glorious.fit 2 points
       let s' = Glorious.fit 1 points
       mapM_ (posgraph . fmap (fromIntegral :: Int -> Double))
             (plotToGraphs $ Plot 32 32 points)
       putStrLn $ "points: " <> show points
       --putStrLn $ "hakaru says: " <> show h -- <> name h
       putStrLn $ "shitty says poly order 2?" <> show s
       putStrLn $ "shitty says poly order 1?" <> show s'
