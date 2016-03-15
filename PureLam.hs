module PureLam where

import Control.Monad
import Control.Monad.Reader
import Data.List
import Prelude hiding (succ)

import Text.PrettyPrint.HughesPJ

import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L
import ParserAux

import Fresher

type Name = String

data Lam = Abs Name Lam
         | App Lam Lam
         | Var Name
         | Print Name

data Val = VUnit 
         | VAbs Name Lam

boundVars :: Lam -> [Name]
boundVars (Abs n l) = n : boundVars l
boundVars (App l1 l2) = boundVars l1 ++ boundVars l2
boundVars _ = []

zero :: Lam
zero = Abs "s" (Abs "z" (Var "z"))

-- this isn't proper eta because we'd need freshness then, this is just a test of something small
etaExp :: Lam -> Lam
etaExp f = Abs "z" (App f (Var "z"))

succ :: Lam -> Lam
succ l = Abs "s" (Abs "z" (App (App l (Var "s")) (Var "z")))

truth :: Lam
truth = Abs "t" (Abs "f" (Var "t"))

falsity :: Lam
falsity = Abs "t" (Abs "f" (Var "f"))

ifThen :: Lam -> Lam -> Lam -> Lam
ifThen l t f = App (App l t) f

instance Show Val where
    show = render . ppVal

ppVal VUnit = text "()"
ppVal (VAbs n l) = ppLam (Abs n l)

ppLam (Abs n l) = parens $ text "\\" <+> text n <> text "." <+> ppLam l
ppLam (App l1 l2) = parens $ ppLam l1 <+>  ppLam l2
ppLam (Var n) = text n
ppLam (Print n) = text "print" <> parens (text n)

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

type Interp = FresherT IO

rename :: Name -> Name -> Lam -> Lam
rename n1 n2 (Print x) = Print x 
rename n1 n2 (Var n) | n == n1 = Var n2
                     | otherwise = Var n
rename n1 n2 (App l1 l2) = App (rename n1 n2 l1) (rename n1 n2 l2)
rename n1 n2 (Abs n l) | n1 == n = Abs n l
                       | otherwise = Abs n (rename n1 n2 l)
substInto n arg (Print n') = return (Print n')
substInto n arg (Var n') | n == n' = return arg
                         | otherwise = return (Var n')
substInto n arg (App l1 l2) = do 
  l1' <- (substInto n arg l1) 
  l2' <- (substInto n arg l2)
  return $ App l1' l2'
substInto n arg (Abs n' l) 
    | n /= n' = do 
        l' <- substInto n arg l
        return $ Abs  n' l'
    | otherwise = do
        alpha <- fresh
        l' <- substInto n arg (rename n alpha l)
        return $ Abs alpha l'
                                         
interpLam :: Lam -> Interp Val
interpLam (Print n) = do
  liftIO $ putStrLn n
  return VUnit
interpLam (Var n) = error "whoops, unbound variable"
interpLam (App l1 l2) = do
  v <- interpLam l1
  case v of
    VAbs n l'' -> do 
           l''' <- (substInto n l2 l'')
           interpLam l'''
    _ -> error "unit applied as a function"
interpLam (Abs n l) = return $ VAbs n l

interpProgram p = runFresherT' (interpLam p) 

interpProgram' p = case runParser parseLam "input" p of
                     Left err -> error (show err)
                     Right p' -> interpProgram p'

testProg1 = "(\\ x. x)"
testProg2 = "((\\ x. x) (\\ y.y))"

testProg3 = ifThen truth (Print "true") (Print "false")
