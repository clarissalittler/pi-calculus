module Fresher where

import Control.Monad.State
import Data.Functor.Identity

type Fresher = FresherT Identity
type FresherT m = StateT ([String],[String]) m

infiniNames = zipWith (\x y -> [x] ++ (show y)) alphas [0..]
    where alphas = ['a'..'z'] ++ alphas

runFresher :: Fresher a -> [String] -> a
runFresher m ss = evalState m (ss,infiniNames)

runFresher' m = runFresher m []

runFresherT :: Monad m => FresherT m a -> [String] -> m a
runFresherT m ss = evalStateT m (ss,infiniNames)

runFresherT' :: Monad m => FresherT m a -> m a
runFresherT' m = runFresherT m []

nix :: Monad m => String -> FresherT m ()
nix s = modify (\(ex,inc) -> (s : ex,inc))

fresh :: Monad m => FresherT m String
fresh = do
  (excl,x : freshy) <- get
  if x `elem` excl 
  then do
    put (excl,freshy)
    fresh
  else do
    put (excl,freshy)
    return x
      
