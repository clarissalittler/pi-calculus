module PiTin where

import qualified PiAST as P
import qualified Tin as T
import Fresher

type Name = String

progToPi :: [T.Decl] -> P.Proc
progToPi ts = makeNames progNames (runFresher (inPar ts) progNames)
    where progNames = map (T.Decl n _ -> n) ts
          makeNames [] p = p
          makeNames (n : ns) p = P.Nu n (makeNames ns p)
          inPar [] = return P.Terminate
          inPar ((T.Decl n ss) : dd) = do 
                 p <- blockToProc n ss
                 ps <- inPar dd
                 return $ P.Par p ps

blockToProc :: String -> [T.Stmt] -> Fresher P.Proc
blockToProc n ss = undefined

stmtToProc :: String -> T.Stmt -> Fresher P.Proc 
stmtToProc n (T.SExp e) = undefined
stmtToProc n (T.SReceive vs) = undefined
stmtToProc n (T.SSend n es) = undefined
stmtToProc n (T.SWhile e ss) = undefined
stmtToProc n (T.SIf e sts sfs) = undefined
