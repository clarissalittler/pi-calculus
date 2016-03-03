module PureLam where

import Control.Monad
import Control.Monad.Reader
import Data.List
import Prelude hiding (succ)

import Text.PrettyPrint.HughesPJ

import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

type Name = String

data Lam = Abs Name Lam
         | App Lam Lam
         | Var Name
         | Print Name

data Val = VUnit 
         | VAbs Name Lam

zero :: Lam
zero = Abs "s" (Abs "z" (Var "z"))

-- this isn't proper eta because we'd need freshness then, this is just a test of something small
etaExp :: Lam -> Lam
etaExp f = Abs "z" (App f (Var "z"))

succ :: Lam -> Lam
succ l = Abs "s" (Abs "z" (App (App l (Var "s")) (Var "z")))


instance Show Val where
    show = render . ppVal

ppVal VUnit = text "()"
ppVal (VAbs n l) = ppLam (Abs n l)

ppLam (Abs n l) = parens $ text "\\" <+> text n <> text "." <+> ppLam l
ppLam (App l1 l2) = parens $ ppLam l1 <+>  ppLam l2
ppLam (Var n) = text n
ppLam (Print n) = text "print" <> parens (text n)

spacey :: Parser ()
spacey = L.space (spaceChar >> return ()) (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")
lexeme = L.lexeme spacey
symbol = L.symbol spacey

many1 p = do
  v <- p
  vs <- many p
  return $ v : vs

name = lexeme (many1 letterChar)
paren = between (symbol "(") (symbol ")")
dot = symbol "."

tries = choice . map try

parseLam = tries [absParse,appParse,
                  printParse, varParse]

absParse = paren $ do
  symbol "\\"
  n <- name
  dot
  l <- parseLam
  return $ Abs n l

appParse = paren $ do
  l1 <- parseLam
  l2 <- parseLam
  return $ App l1 l2

varParse = fmap Var name

printParse = do
  symbol "print"
  n <- paren name
  return $ Print n

instance Show Lam where
    show = render . ppLam

type Env a = [(Name,a)]

type Interp = ReaderT (Env Lam) IO

lookupM :: Name -> Interp Lam
lookupM n = do
  env <- ask
  case lookup n env of
    Nothing -> error "no such variable defined" -- might want to change this into just a variable evaluating to itself
    Just l -> return l

withVar :: Name -> Lam -> Interp a -> Interp a    
withVar n l = local ((n,l):)

interpLam :: Lam -> Interp Val
interpLam (Print n) = do
  liftIO $ putStrLn n
  return VUnit
interpLam (Var n) = do
  l <- lookupM n
  interpLam l
interpLam (App l1 l2) = do
  v <- interpLam l1
  case v of
    VAbs n l'' -> withVar n l2 $ interpLam l''
    _ -> error "unit applied as a function"
interpLam (Abs n l) = return $ VAbs n l

interpProgram p = runReaderT (interpLam p) []

interpProgram' p = case runParser parseLam "input" p of
                     Left err -> error (show err)
                     Right p' -> interpProgram p'

testProg1 = "(\\ x. x)"
testProg2 = "((\\ x. x) (\\ y.y))"
