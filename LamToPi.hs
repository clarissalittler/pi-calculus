module LamToPi where

import qualified PurePi as P
import qualified PureLam as L
import Fresher

lamToPi :: L.Lam -> P.Proc
lamToPi l = runFresher (transLam' l) (L.boundVars l)

type Name = String

transLam' l = do
  z <- fresh
  transLam l z

transLam :: L.Lam -> Name -> Fresher P.Proc
transLam (L.Print n) _ = return $ P.Print n
transLam (L.Var x) n = return $ P.Send x n P.Terminate
transLam (L.Abs x b) n = do
  u <- fresh
  b' <- transLam b u
  return $ P.Receive n x $ P.Receive n u $ b'
transLam (L.App f a) n = do
  c <- fresh
  d <- fresh
  v <- fresh
  f' <- transLam f c 
  a' <- transLam a v
  return $ P.Nu c $ P.Nu d $ P.Par (P.Par f' 
                                         (P.Send c d $ P.Send c n $ P.Terminate))
                               (P.Serv $ P.Receive d v a')
