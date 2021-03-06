module Tin where

import Control.Concurrent
import Control.Monad hiding (forM)
import System.IO
import Control.Monad.State hiding (forM)
import Data.List

import Text.PrettyPrint.HughesPJ
import ParserAux
import Exp

import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

{- To add locks to the language I suppose I could switch over to STM, or I could use semaphores. Why don't I just use semaphores? -} 

-- Pretty Printing

ppStmt :: Stmt -> Doc
ppStmt (SExp e) = ppExp e
ppStmt (SReceive vs) = text "receive" <+> (parens $ sep $ punctuate comma $ map text vs)
ppStmt (SSend n es) = text "send" <+> ppExp n <+> (parens $ sep $ punctuate comma $ map ppExp es)
ppStmt (SWhile e ss) = text "while" <+> parens (ppExp e) <+> hang (text "do") 5 (braces $ hsep $ map ppStmt ss)
ppStmt (SIf e sts sfs) = text "if" <+> parens (ppExp e) $+$ hang (text "then") 5 (braces $ hsep $ map ppStmt sts) 
                         $+$ hang (text "else") 5 (braces $ hsep $ map ppStmt sfs)
ppStmt (SLock e bs) = text "lock" <+> ppExp e <+> nest 5 (braces $ hsep $ map ppStmt bs)

ppDecl :: Decl -> Doc
ppDecl (Decl n ss) = text n <+> hang (text ":=") 5 (vcat $ map ppStmt ss)

instance Show Decl where
    show = render . ppDecl

instance Show Stmt where
    show = render . ppStmt

-- Parsing

parseStmt = tries [parseReceive,
                   parseSend,
                   parseWhile,
                   parseIf,
                   parseLock,
                   fmap SExp parseExp]

parseBlock = bracey $ many1 parseStmt

parseLock = do
  symbol "lock"
  n <- parseExp
  block <- parseBlock
  return $ SLock n block

parseReceive = do
  symbol "receive"
  vs <- paren $ commaSep name
  return $ SReceive vs
parseSend = do
  symbol "send"
  n <- parseExp
  es <- paren $ commaSep parseExp
  return $ SSend n es
parseWhile = do
  symbol "while"
  cond <- paren parseExp
  symbol "do"
  ss <- parseBlock
  return $ SWhile cond ss
parseIf = do
  symbol "if"
  cond <- paren parseExp
  symbol "then"
  trues <- parseBlock
  symbol "else"
  falses <- parseBlock
  return $ SIf cond trues falses

parseDecl = do
  n <- name
  symbol ":="
  ss <- parseBlock
  return $ Decl n ss

parseProg = many1 parseDecl

--
data Stmt = SExp Exp
          | SReceive [Var]
          | SLock Exp [Stmt] -- within the scope of this block have an exclusive lock on the mailbox for
                             -- the named process
          | SSend Exp [Exp]
          | SWhile Exp [Stmt]
          | SIf Exp [Stmt] [Stmt]

data Decl = Decl Name [Stmt]

data Inbox = Inbox {sem :: QSem, chan :: (Chan Val)}

data InterpEnv = IE { inboxes :: NEnv Inbox, -- inboxes
                      venv :: VEnv Val, -- value env
                      outc :: Chan String, -- output queue
                      self :: Name}

type Interp = StateT InterpEnv IO

putText :: String -> Interp ()
putText s = do
  c <- gets outc
  liftIO $ writeChan c s

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

                       
addVars :: Var -> Val -> Interp ()
addVars vr vl = modify $ \ie -> let ve = venv ie
                                    ve' = (vr,vl):ve
                                in ie{venv=ve'}

forM :: Monad m => Int -> m a -> m [a]
forM 0 m = fmap (\x -> [x]) m
forM i m = do
  a <- m
  as <- forM (i-1) m
  return $ a : as

interpExp :: Exp -> Interp Val
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
interpExp (EInt i) = return $ VInt i
interpExp (EString s) = return $ VString s
interpExp (EBool b) = return $ VBool b
interpExp EUnit = return VUnit
interpExp (EVar v) = do
  ev <- gets venv
  case lookup v ev of
    Nothing -> error "unknown variable"
    Just x -> return x
interpExp (EPrint e) = do
  v <- interpExp e
  putText $ show v
  return VUnit
interpExp (EName n) = return $ VName n
  
interpStmt :: Stmt -> Interp ()
interpStmt (SExp e) = interpExp e >> return ()
interpStmt (SReceive vs) = do
  n <- gets self
  c <- lookupInbox n
  vals <- forM (length vs - 1) (liftIO $ readChan $ chan c)
  mapM_ (uncurry addVars) $ zip vs vals
interpStmt (SIf e1 st sf) = do
  v <- interpExp e1
  case v of
    VBool b -> if b then interpSeq st else interpSeq sf
    _ -> error "type error: non-boolean used in if statement"
interpStmt (SSend ne es) = do
  ne' <- interpExp ne
  case ne' of
    VName n -> do
             c <- chan `fmap` lookupInbox n
             vals <- mapM interpExp es
             liftIO $ writeList2Chan c vals
    _ -> error "can't send to a non-name"
interpStmt (SWhile e s) = do
  v <- interpExp e
  case v of
    VBool b -> if b then interpSeq s >> interpStmt (SWhile e s) else return ()
    _ -> error "type error: non-boolean used in while statement"
interpStmt (SLock e b) = do
  v <- interpExp e
  case v of
    VName n -> do
           (Inbox sem c) <- lookupInbox n
           liftIO $ waitQSem sem
           interpSeq b
           liftIO $ signalQSem sem
    _ -> error "can't send to a non-name"

loop :: Monad m => m () -> m ()
loop d = d >> (loop d)

outThread c = loop $ do
                w <- readChan c
                putStrLn w

makeInbox :: Decl -> IO (Name,Inbox)
makeInbox (Decl n _) = do
  m <- newChan
  q <- newQSem 1
  return (n,Inbox q m)

interpSeq :: [Stmt] -> Interp ()
interpSeq = mapM_ interpStmt

forkIO' :: IO a -> IO ThreadId
forkIO' m = forkIO $ m >> return ()

runFile f = do
  progText <- readFile f
  case parse parseProg f progText of
    Left e -> error $ show e
    Right p -> runProg p

runExp :: Exp -> IO ()
runExp e = do
  outChan <- newChan
  forkIO $ outThread outChan
  runStateT (interpExp e) (IE [] [] outChan "self")
  return ()

runProg :: [Decl] -> IO ()
runProg ds = do
  outChan <- newChan
  forkIO $ outThread outChan
  inboxes <- mapM makeInbox ds
  mapM_ (\(Decl n ss) -> forkIO' $ runStateT (interpSeq ss) (IE inboxes [] outChan n)) ds
