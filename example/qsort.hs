import           Data.Monoid
import           Data.List
import           System.Console.Ansigraph
import           Control.Arrow

import           Test.QuickCheck
import           Test.BigOh
import qualified Test.BigOh.Fit.Hakaru as H
import qualified Test.BigOh.Fit.Naive  as Glorious
import qualified Data.Discrimination   as D
main
  = do x      <- generate
               $ genWhnf 20 (50,2000) (\n -> take n <$> infiniteList)
                                      (sort :: [Int] -> [Int])
       reps <- runInputs benchmarkConfig x
       let points = first fromIntegral <$> getTimes reps
       let epsilon = minimum $ getStdDevs reps
       --let points = [(50.0,4.333847980311836e-5),(65.0,7.271578817073304e-5),(80.0,1.0817448289322176e-4),(95.0,1.476343443334346e-4),(110.0,1.9401723390177167e-4),(125.0,2.5757683265656395e-4),(140.0,3.211461530435924e-4),(155.0,4.019078051066144e-4),(170.0,4.813139173106054e-4),(185.0,7.45060456346182e-4)]
       -- let points = 
       --      [(1.0,6.743924314060812e-8),(2.0,1.6881121057872632e-7),(3.0,3.784447133477605e-7),(4.0,5.268417214273512e-7),(5.0,7.208420000555227e-7),(6.0,1.0330625234336618e-6),(7.0,1.1740516548963642e-6),(8.0,1.4621713747220158e-6),(9.0,1.8786935349889448e-6),(10.0,2.1062272623853422e-6),(11.0,2.6097668615374246e-6),(12.0,2.9521839563186653e-6),(13.0,3.876924684530011e-6),(14.0,3.556049573549235e-6),(15.0,4.946794953211465e-6),(16.0,4.827297294154682e-6),(17.0,5.663898284912599e-6),(18.0,5.798772419362704e-6),(19.0,6.792859656886842e-6)]
       -- let points = [(1.0,3.167386486797687e-8),(2.0,8.062574601155624e-8),(3.0,8.77738634654382e-8),(4.0,1.8236334861313467e-7),(5.0,2.5339146896927717e-7),(6.0,2.989270471462668e-7),(7.0,3.0723605217850713e-7),(8.0,3.8771295983283255e-7),(9.0,4.128735730849073e-7),(10.0,4.656314746044062e-7),(11.0,5.888679567094566e-7),(12.0,5.211566539907106e-7),(13.0,6.462910630622821e-7),(14.0,6.311127960837393e-7),(15.0,6.547534481453109e-7),(16.0,9.476858842859062e-7),(17.0,8.76069769938057e-7),(18.0,9.13435171974278e-7),(19.0,9.505668031036458e-7)]
       -- let points = take 100 [ (x,x*x) | x <- [1..]]
       --h      <- H.fit H.quadratic H.knownOrders (fmap fst points) (fmap snd points)
       let s3  = Glorious.fit 3 points
       let s2  = Glorious.fit 2 points
       let s1  = Glorious.fit 1 points
       let s0  = Glorious.fit 0 points
       mapM_ (posgraph . fmap (fromIntegral :: Int -> Double))
             (plotToGraphs $ Plot 32 32 points)
       --putStrLn $ "hakaru says: " <> show h
       putStrLn $ "shitty says poly order 3? " <> show s3
       putStrLn $ "shitty says poly order 2? " <> show s2
       putStrLn $ "shitty says poly order 1? " <> show s1
       putStrLn $ "shitty says poly order 0? " <> show s0
       putStrLn $ "shitty says  " <> show (Glorious.polyOrder epsilon points)
       print $ fmap fst points
       print $ fmap snd points

selection :: (Ord a) => [a] -> [a]
selection [] = []
selection xs = let x = maximum xs in selection (delete x xs) ++ [x] 

{-
main
  = void $ complexityWhnf (\n -> take n <$> infiniteList)
                          (selection :: [Int] -> [Int])
                          20 (50, 2000)
                          (== 2)
-}
