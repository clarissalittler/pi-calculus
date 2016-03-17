module PiTin where

import qualified PiAST as P
import qualified Tin as T
import Fresher
import Exp

type Name = String

progToPi :: [T.Decl] -> P.Proc
progToPi ts = makeNames progNames (runFresher (inPar ts) progNames)
    where progNames = map (\ (T.Decl n _) -> n) ts
          makeNames [] p = p
          makeNames (n : ns) p = P.Nu n (makeNames ns p)
          inPar [] = return P.Terminate
          inPar ((T.Decl n ss) : dd) = do 
                 p <- blockToProc n ss P.Terminate
                 ps <- inPar dd
                 return $ P.Par p ps

blockToProc :: String -> [T.Stmt] -> P.Proc -> Fresher P.Proc
blockToProc n [] p = return p
blockToProc n (s : ss) p = do 
  sp <- blockToProc n ss p
  stmtToProc n s sp

stmtToProc :: String -> T.Stmt -> P.Proc -> Fresher P.Proc 
stmtToProc n (T.SExp e) p = do
  n <- fresh
  y <- fresh
  return $ P.Nu n $ P.Par (P.Send (EName n) e P.Terminate) (P.Receive (EName n) y p) 
stmtToProc n (T.SReceive vs) p = makeReceive n vs p
    where makeReceive n [] p = return p
          makeReceive n (x : xs) p = do
            nix x
            p' <- makeReceive n xs p
            return $ P.Receive (EName n) x p'
stmtToProc n (T.SSend to es) p = makeSenders n es p
    where makeSenders n [] p = return p
          makeSenders n (e : es) p = do
            p' <- makeSenders n es p
            return $ P.Send to e p'           
stmtToProc n (T.SWhile e ss) p = do
  sp <- blockToProc n ss P.Terminate
  conn <- fresh
  dummy <- fresh
  return $ P.Par (P.Serv $ P.If e sp (P.Send (EName conn) EUnit P.Terminate)) (P.Receive (EName conn) dummy p)
stmtToProc n (T.SIf e sts sfs) p = do
  tp <- blockToProc n sts p 
  fp <- blockToProc n sfs p 
  return $ P.If e tp fp
