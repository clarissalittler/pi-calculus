module PiAST where

import Control.Concurrent
import Control.Monad
import System.IO
import Control.Monad.Reader

import Text.PrettyPrint.HughesPJ

type Name = String
type Var = String

data Value = VName Name
           | VInt Int
           | VUnit
           deriving (Read, Show)

ppVal (VName n) = text $ "@" ++ n
ppVal (VInt i) = int i
ppVal (VUnit) = lparen <> rparen

ppProc (Receive x y p) = text ("@" ++ x) <> parens (text y) <> text "." <> ppProc p
ppProc (Send n e p) = text ("@" ++ n) <> parens (ppExp e) <+> ppProc p
ppProc (Par p1 p2) = ppProc p1 <+> text "|" <+> ppProc p2
ppProc (Nu n p) = text ("nu @" ++ n ++ ".") <+> ppProc p
ppProc (Serv p) = text "!" <> ppProc p
ppProc Terminate = text "end"

ppExp (EWrite e) = text "write" <> parens (ppExp e)
ppExp (EAdd e1 e2) = ppExp e1 <+> text "+" <+> ppExp e2
ppExp (ENeg e) = text "-" <> ppExp e
ppExp (EMult e1 e2) = ppExp e1 <+> text "*" <+> ppExp e2
ppExp (EVal v) = ppVal v
ppExp (EVar n) = text n

data Proc = Receive Name Name Proc
          | Send Name Exp Proc
          | Par Proc Proc
          | Nu Name Proc
          | Serv Proc
          | Terminate

data Exp = EWrite Exp
         | EAdd Exp Exp
         | ENeg Exp
         | EMult Exp Exp
         | EVal Value 
         | EVar Name


type NEnv a = [(Name,a)]
type VEnv a = [(Var,a)]

data InterpEnv = IE { menv :: NEnv (MVar Value), -- inboxes
                      venv :: VEnv Value, -- value env
                      outc :: Chan String, -- output queue
                      inc :: Chan String} -- input queue

type Interp = ReaderT InterpEnv IO

forkM :: Interp a -> Interp ()
forkM x = do
  e <- ask
  liftIO $ forkIO $ (runReaderT x e >> return ())
  return ()

withInbox :: Name -> MVar Value -> Interp a -> Interp a
withInbox n m = local $ \e -> let ms = menv e 
                             in e{menv = ((n,m) : ms)}

withVal :: Name -> Value -> Interp a -> Interp a
withVal n v = local $ \e -> let vn = venv e
                            in e{venv = ((n,v) : vn)}

lookupMVar :: Name -> Interp (MVar Value)
lookupMVar n = do
  env <- asks menv
  case lookup n env of
    Nothing -> error "MVar doesn't exist"
    Just m -> return m

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
  withInbox n m $ interpProc p
interpProc (Send n e p) = do
  m <- lookupMVar n
  v <- interpExp e
  liftIO $ putMVar m v
  interpProc p
interpProc (Receive c y p) = do
  m <- lookupMVar c
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
  ve <- asks venv
  case lookup n ve of
    Nothing -> error "unbound variable"
    Just x -> return x

readerThread c = do
  w <- readChan c
  putStrLn w
  readerThread c
