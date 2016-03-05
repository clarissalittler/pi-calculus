module Tin where

import Control.Concurrent
import Control.Monad
import System.IO
import Control.Monad.Reader
import Data.List

import Text.PrettyPrint.HughesPJ

import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

type Name = String
type Value = Name
type Op = String
type Var = String

data Exp = EBinOP Exp Op Exp
         | EUnOp Op Exp
         | EInt Int
         | EBool Bool
         | EString String
         | EVar Var
         | EUnit
         | ERead
         | EName String
         | EPrint Exp
         
data Stmt = SExp Exp
          | SReceive [Var]
          | SSend Name [Exp]
          | SWhile Exp Stmt
          | SIf Exp Stmt Stmt
          | SSeq Stmt Stmt
          | SSkip -- we don't parse this one it's just for internal use

data Val = VInt Int
         | VString String
         | VName Name
         | VBool Bool
         | VUnit 
         

type NEnv a = [(Name,a)]
type VEnv a = [(Var,a)]

type Inbox = Chan Val

data InterpEnv = IE { inboxes :: NEnv Inbox, -- inboxes
                      venv :: VEnv Val, -- value env
                      outc :: Chan String, -- output queue
                      inc :: Chan String,
                      self :: Name} -- input queue

type Interp = StateT InterpEnv IO
                       
addVars :: Var -> Val -> Interp ()
addVars = undefined

putText :: String -> Interp ()
putText s = do
  c <- gets outc
  liftIO $ writeChan c s

readText :: Interp String
readText = do
  c <- gets inc
  liftIO $ readChan c

forkM :: Interp a -> Interp ()
forkM x = do
  e <- get
  liftIO $ forkIO $ runStateT x e >> return ()
  return ()

lookupInbox :: Name -> Interp Inbox
lookupInbox n = do
  i <- gets inboxes
  case lookup n i of
    Nothing -> error "unknown name in message"
    Just c -> return c

forM :: Monad m => Int -> m a -> m [a]
forM 0 m = fmap (\x -> [x]) m
forM i m = do
  a <- m
  as <- forM (i-1) m
  return $ a : as

binOpTable :: [(String,Exp -> Exp -> Exp)]
binOpTable = undefined

interpExp :: Exp -> Interp Val
interpExp (EInt i) = return $ VInt i
interpExp (EString s) = return $ VString s
interpExp (EBool b) = return $ VBool b
interpExp EUnit = return VUnit
interpExp (EVar v) = do
  ev <- gets venv
  case lookup v ev of
    Nothing -> error "unknown variable"
    Just x -> return x
interpExp ERead = fmap EName readText
interpExp (EPrint e) = do
  v <- interpExp e
  
interpStmt :: Stmt -> Interp ()
interpStmt (SExp e) = interpExp e >> return ()
interpStmt SSkip = return ()
interpStmt (SReceive vs) = do
  n <- gets self
  c <- lookupInbox n
  vals <- forM (length vs - 1) (readChan c)
  mapM addVars $ zip vs vals
interpStmt (SSeq s1 s2) = do
  interpStmt s1
  interpStmt s2
interpStmt (SIf e1 st sf) = do
  v <- interpExp e1
  case v of
    VBool b -> if b then interpStmt st else interpStmt sf
    _ -> error "type error: non-boolean used in if statement"
interpStmt (SSend n es) = do
  c <- lookupInbox n
  vals <- mapM interpExp es
  writeList2Chan c vals
interpStmt (SWhile e s) = do
  v <- interpExp e
  case v of
    VBool b -> if b then interpStmt s >> interpStmt (SWhile e s) else return ()
    _ -> error "type error: non-boolean used in while statement"
