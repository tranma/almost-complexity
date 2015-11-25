{-# LANGUAGE RankNTypes, MultiParamTypeClasses,FlexibleInstances, FlexibleContexts #-}
import           Criterion.Types
import qualified Data.Discrimination as D
import           Data.Ix
import           Data.List

import           Test.BigOh
import qualified Test.BigOh.Fit.R    as R
import Control.Monad
import Prelude hiding (abs)

main
  = rVoodoo
  $ do header "free:"
       go (== 2) (== R.Quadratic) (lengthTrace . slowRunRevEcho)
       header  "codensity:"
       go (== 1) (== R.Linear) (lengthTrace . fastRunRevEcho)
  where
   go i o f
     = do x <- genRunWhnf (defaultsForListInput :: Settings [Char])
                             { generator = return . genInputSized
                             , benchConf = defaultBenchmarkConfig { timeLimit = 2 }
                             , sampleRange = (10,2000)
                             , numSamples = 10
                             }
                          f
          graphPoints $ getPoints x
          header "Naive:"
          naive i x
          header  "R:"
          rlm   o x



data Free f a = Return a | Wrap (f (Free f a))

instance Functor f => Functor (Free f) where 
  fmap f x = x >>= return . f

instance Functor f => Applicative (Free f) where
  pure = Return
  (<*>) = ap

instance Functor f => Monad (Free f) where 
  return = Return
  Return a >>= k = k a
  Wrap t >>= k = Wrap (fmap (>>= k) t)

newtype C m a = C (forall b. (a -> m b) -> m b)

rep :: Monad m => m a -> C m a
rep m = C (m >>=)

abs :: Monad m => C m a -> m a
abs (C p) = p return

instance Functor (C m) where
  fmap f x = x >>= return . f
instance Applicative (C m) where
  pure = return
  (<*>) = ap

instance Monad (C m) where 
  return a = C ($ a)
  C r >>= k = C (\h -> r (\a -> case k a of C q -> q h))

class (Functor f, Monad m) => FreeLike f m where 
  wrap :: f (m a) -> m a

instance Functor f => FreeLike f (Free f) where 
  wrap = Wrap

instance FreeLike f m => FreeLike f (C m) where 
  wrap t = C (\h -> wrap (fmap (\(C r) -> r h) t))


improve :: Functor f => (forall m. FreeLike f m => m a) -> Free f a
improve m = abs m



data FakeIO n = GetChar (Char -> n) 
              | PutChar Char n
instance Functor FakeIO where 
  fmap f (GetChar c) = GetChar (fmap f c)
  fmap f (PutChar c n) = PutChar c (f n)

getChar' :: FreeLike FakeIO m => m Char
getChar' = wrap (GetChar return)

putChar' :: FreeLike FakeIO m => Char -> m ()
putChar' c = wrap (PutChar c (return ()))



revEcho :: FreeLike FakeIO m => m ()
revEcho = do
  c <- getChar'
  when (c /= ' ') $ do
    revEcho
    putChar' c

lengthTrace :: Trace a -> Int
lengthTrace (Read t) = lengthTrace t
lengthTrace (Print _ t) = 1 + lengthTrace t
lengthTrace _ = 0

data Trace a = Read (Trace a) | Print Char (Trace a) | Finish a deriving (Show)

run :: Free FakeIO a -> [Char] -> Trace a
run (Return a) cs = Finish a
run (Wrap (GetChar f)) (c:cs) = Read (run (f c) cs) 
run (Wrap (PutChar c f)) cs = Print c (run f cs) 

slowRunRevEcho :: [Char] -> Trace ()
slowRunRevEcho = run (revEcho :: Free FakeIO ())

fastRunRevEcho :: [Char] -> Trace ()
fastRunRevEcho = run (improve revEcho)

genInputSized :: Int -> [Char]
genInputSized n = take n (cycle "abcdefgh") ++ " " 
