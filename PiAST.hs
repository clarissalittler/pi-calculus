module PiAST where

import Control.Concurrent
import Control.Monad
import System.IO
import Control.Monad.Reader

type Name = String

data Value = VName Name
           | VInt Int
           | VUnit
           deriving (Read, Show)

data Proc = Receive Name Name Proc
          | Send Name Exp Proc
          | Par Proc Proc
          | Nu Name Proc
          | Serv Proc
          | Terminate

data Exp = EWrite Exp
         | ERead
         | EAdd Exp Exp
         | ENeg Exp
         | EMult Exp Exp
         | EVal Value -- there's a distinction here between names-as-values and names-as-variables, not sure how to present that in the syntax
         | EVar Name

type Env a = [(Name,a)]

type Interp = ReaderT (Env (MVar Value), Env Value) IO

forkM :: Interp a -> Interp ()
forkM x = do
  e <- ask
  liftIO $ forkIO $ (runReaderT x e >> return ())
  return ()

withChan :: Name -> MVar Value -> Interp a -> Interp a
withChan n m = local (\(ec,ev) -> ((n,m) : ec,ev))

withVal :: Name -> Value -> Interp a -> Interp a
withVal n v = local (\(ec,ev) -> (ec, (n,v): ev))

interpProc :: Proc -> Interp ()
interpProc Terminate = return ()
interpProc (Par p1 p2) = do
  forkM $ interpProc p1
  interpProc p2
interpProc (Serv p) = do
  forkM $ interpProc p
  interpProc (Serv p)
interpProc (Nu n p) = do
  m <- liftIO $ newEmptyMVar
  withChan n m $ interpProc p
interpProc (Send n e p) = do
  env <- asks fst
  v <- interpExp e
  case lookup n env of
    Nothing -> error "channel doesn't exist"
    Just m -> (liftIO $ putMVar m v) >> interpProc p
interpProc (Receive c y p) = do
  ec <- asks fst
  case lookup c ec of
    Nothing -> error "channel doesn't exist"
    Just m -> do
      v <- liftIO $ readMVar m
      withVal y v $ interpProc p

liftNumV :: (Int -> Int) -> Value -> Value
liftNumV op (VInt i) = VInt (op i)
liftNumV _ _ = error "type error in numeric operation"

liftNumV2 :: (Int -> Int -> Int) -> Value -> Value -> Value
liftNumV2 op (VInt i) (VInt j) = VInt $ op i j
liftNumV2 _ _ _ = error "type error in numeric operation"

interpExp :: Exp -> Interp Value
interpExp (EWrite e) = do 
  v <- interpExp e
  liftIO (print v)
  return v
interpExp ERead = liftIO readLn
interpExp (EAdd e1 e2) = do
  v1 <- interpExp e1
  v2 <- interpExp e2
  return $ liftNumV2 (+) v1 v2
interpExp (ENeg e) = interpExp e >>= (return . liftNumV (\x -> -x))
interpExp (EMult e1 e2) = do
  v1 <- interpExp e1
  v2 <- interpExp e2
  return $ liftNumV2 (+) v1 v2
interpExp (EVal v) = return v
interpExp (EVar n) = do
  (_,venv) <- ask
  case lookup n venv of
    Nothing -> error "unbound variable"
    Just x -> return x
