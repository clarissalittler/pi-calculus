module PurePi where

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

data Proc = Receive Name Name Proc
          | Send Name Name Proc
          | Par Proc Proc
          | Nu Name Proc
          | Serv Proc
          | Print Name
          | Terminate

type Env a = [(Name,a)]

type Interp = ReaderT (Env (MVar Value)) IO

ppProc (Receive x y p) = text x <> parens (text y) <> text "." <> ppProc p
ppProc (Send n v p) = text ("@" ++ n) <> parens (text v) <+> ppProc p
ppProc (Par p1 p2) = parens $ ppProc p1 <+> text "|" <+> ppProc p2
ppProc (Nu n p) = text ("nu " ++ n ++ ".") <+> ppProc p
ppProc (Serv p) = text "!" <> ppProc p
ppProc Terminate = text "end"
ppProc (Print n) = text "print" <> parens (text n)

-- parsing code 
spacey :: Parser ()
spacey = L.space (spaceChar >> return ()) (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")
lexeme = L.lexeme spacey
symbol = L.symbol spacey

many1 p = do
  vs <- many p
  v <- p
  return $ vs ++ [v]

name = lexeme (many1 letterChar)
paren = between (symbol "(") (symbol ")")
dot = symbol "."

parseProc = sendParse 
            <|> receiveParse 
            <|> parParse 
            <|> nuParse 
            <|> termParse
            <|> serveParse
            <|> printParse

sendParse = do 
  symbol "@"
  x <- name
  y <- paren name
  dot
  p <- parseProc
  return $ Send x y p
receiveParse = do
  x <- name
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
printParse = do
  symbol "print"
  v <- paren $ name
  return $ Print v


forkM :: Interp a -> Interp ()
forkM x = do
  e <- ask
  liftIO $ forkIO $ (runReaderT x e >> return ())
  return ()

withChan :: Name -> MVar Value -> Interp a -> Interp a
withChan n m = local (\ec -> (n,m) : ec)

nameChange x y z = if z == x then y else z

freeVars :: Proc -> [Name]
freeVars Terminate = []
freeVars (Serv p) = freeVars p
freeVars (Nu n p) = freeVars p \\ [n]
freeVars (Par p1 p2) = freeVars p1 ++ freeVars p2
freeVars (Send x y p) = freeVars p ++ [x,y]
freeVars (Receive x y p) = freeVars p ++ [x]
freeVars (Print n) = [n]

substName :: Name -> Name -> Proc -> Proc
substName x y (Print n) = Print (nameChange x y n)
substName x y Terminate = Terminate
substName x y (Par p1 p2) = Par (substName x y p1) (substName x y p2)
substName x y (Serv p) = Serv (substName x y p)
substName x y (Send c v p) =  Send (nameChange x y c) (nameChange x y v) (substName x y p)
substName x y (Receive c v p) | x /= v && y /= v = Receive c v (substName x y p) 
                              | otherwise = Receive c v p  
substName x y (Nu n p) | x /= n && y /= n = Nu n (substName x y p)
                       | otherwise = Nu n p 
-- I'm not even bothering with alpha renaming but instead if the freshness condition isn't satisfied it just doesn't substitute at all
-- maybe that's a cheap way to deal, I'm not sure how much I care at the moment

interpProc :: Proc -> Interp ()
interProc (Print n) = liftIO $ putStrLn n
interpProc Terminate = liftIO $ putStrLn "Thread terminated"
interpProc (Par p1 p2) = do
  forkM $ interpProc p1
  interpProc p2
interpProc (Serv p) = do
  liftIO $ threadDelay 2000
  forkM $ interpProc p
  interpProc (Serv p)
interpProc (Nu n p) = do
  m <- liftIO newEmptyMVar
  withChan n m $ interpProc p
interpProc (Send x y p) = do
  env <- ask
  case lookup x env of
    Nothing -> error "channel doesn't exist"
    Just m -> (liftIO $ putMVar m y) >> interpProc p
interpProc (Receive x y p) = do
  ec <- ask
  case lookup x ec of
    Nothing -> error "channel doesn't exist"
    Just m -> do
      v <- liftIO $ readMVar m
      interpProc $ substName y v p
