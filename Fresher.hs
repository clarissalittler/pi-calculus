module Fresher where

import Control.Monad.State

type Fresher = State ([String],[String])

infiniNames = zipWith (\x y -> [x] ++ (show y)) alphas [0..]
    where alphas = ['a'..'z'] ++ alphas

runFresher :: Fresher a -> [String] -> a
runFresher m ss = evalState m (ss,infiniNames)

runFresher' m = runFresher m []

nix :: String -> Fresher ()
nix s = modify (\(ex,inc) -> (s : ex,inc))

fresh :: Fresher String
fresh = do
  (excl,x : freshy) <- get
  if x `elem` excl 
  then do
    put (excl,freshy)
    fresh
  else do
    put (excl,freshy)
    return x
      
