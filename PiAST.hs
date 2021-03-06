module PiAST where

import Control.Concurrent
import Control.Monad
import System.IO
import Control.Monad.Reader
import ParserAux
import Exp
import Fresher

import Text.PrettyPrint.HughesPJ
import Text.Megaparsec
import Text.Megaparsec.String


ppProc (Receive x y p) = ppExp x <> parens (text y) <> text "." <> ppProc p
ppProc (Send n e p) = text "&" <+> ppExp n <> parens (ppExp e) <> text "." <+> ppProc p
ppProc (Par p1 p2) = ppProc p1 <+> text "|" <+> ppProc p2
ppProc (Nu n p) = text ("nu @" ++ n ++ ".") <+> ppProc p
ppProc (Serv p) = text "!" <> ppProc p
ppProc (If e1 p1 p2) = text "if" <> parens (ppExp e1) <> text "then" <+> parens (ppProc p1) <+> text "else" <+> parens (ppProc p2)
ppProc Terminate = text "end"

data Proc = Receive Exp Name Proc
          | Send Exp Exp Proc
          | Par Proc Proc
          | Nu Name Proc
          | Serv Proc
          | If Exp Proc Proc
          | Terminate


parseProc = tries [sendParse,
                   receiveParse,
                   parParse,
                   nuParse,
                   termParse,
                   serveParse,
                   ifParse]
ifParse = do
  symbol "if"
  e <- paren parseExp
  symbol "then"
  pt <- paren parseProc
  symbol "else"
  pf <- paren parseProc
  return $ If e pt pf

sendParse = do 
  symbol "&"
  x <- parseExp 
  e <- paren parseExp
  dot
  p <- parseProc
  return $ Send x e p
receiveParse = do
  x <- parseExp
  y <- paren name
  dot
  p <- parseProc
  return $ Receive x y p
parParse = paren $ do
             p1 <- parseProc
             symbol "|"
             p2 <- parseProc
             return $ Par p1 p2
nuParse = do
  symbol "nu"
  x <- name
  dot
  p <- parseProc
  return $ Nu x p
termParse = symbol "end" >> return Terminate
serveParse = do
  symbol "!"
  p <- parseProc
  return $ Serv p

data InterpEnv = IE { menv :: NEnv (MVar Val), -- inboxes
                      venv :: VEnv Val, -- value env
                      outc :: Chan String} -- input queue

type Interp = ReaderT InterpEnv IO

forkM :: Interp a -> Interp ()
forkM x = do
  e <- ask
  liftIO $ forkIO $ (runReaderT x e >> return ())
  return ()

withInbox :: Name -> MVar Val -> Interp a -> Interp a
withInbox n m = local $ \e -> let ms = menv e 
                             in e{menv = ((n,m) : ms)}

withVal :: Name -> Val -> Interp a -> Interp a
withVal n v = local $ \e -> let vn = venv e
                            in e{venv = ((n,v) : vn)}

lookupMVar :: Name -> Interp (MVar Val)
lookupMVar n = do
  env <- asks menv
  case lookup n env of
    Nothing -> error "MVar doesn't exist"
    Just m -> return m

interpProc :: Proc -> Interp ()
interpProc (If e pt pf) = do
  v <- interpExp e
  case v of
    VBool b -> if b then interpProc pt else interpProc pf
    _ -> error "type error in If statement"
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
interpProc (Send e1 e2 p) = do
  nn <- interpExp e1
  case nn of
    VName n -> do
            m <- lookupMVar n
            v <- interpExp e2
            liftIO $ putMVar m v
            interpProc p
    _ -> error "expression didn't evaluate to name in Send"
interpProc (Receive c y p) = do
  nn <- interpExp c
  case nn of
    VName n -> do
            m <- lookupMVar n
            v <- liftIO $ readMVar m
            withVal y v $ interpProc p
    _ -> error "expression didn't evaluate to name in Receive"

interpExp :: Exp -> Interp Val
interpExp EUnit = return VUnit
interpExp (EPrint e) = do 
  v <- interpExp e
  liftIO (print v)
  return VUnit
interpExp (EBool True) = return $ VName "true"
interpExp (EBool False) = return $ VName "false"
interpExp (EBinOp e1 op e2) = do
  v1 <- interpExp e1
  v2 <- interpExp e2
  case lookup op binOpTable of
    Nothing -> error "unknown operation"
    Just op' -> return $ op' v1 v2
interpExp (EUnOp op e) = do
  v <- interpExp e
  case lookup op unOpTable of
    Nothing -> error "unknown operation"
    Just op' -> return $ op' v
interpExp (EVar n) = do
  ve <- asks venv
  case lookup n ve of
    Nothing -> error "unbound variable"
    Just x -> return x
interpExp (EInt i) = return $ VInt i
interpExp (EString s) = return $ VString s
interpExp (EName n) = return $ VName n

readerThread c = do
  w <- readChan c
  putStrLn w
  readerThread c

interpProgram' prog = case runParser parseProc "input" prog of
                        Left err -> error (show err)
                        Right p -> interpProgram p

interpProgram prog = do
  outChan <- newChan
  forkIO $ readerThread outChan
  runReaderT (interpProc prog) (IE [] [] outChan)

runFile filename = readFile filename >>= interpProgram'
