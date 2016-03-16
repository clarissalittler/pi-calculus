module Exp where

import Control.Concurrent
import Control.Monad hiding (forM)
import System.IO
import Control.Monad.State hiding (forM)

import Text.PrettyPrint.HughesPJ
import ParserAux

import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

type Op = String
type Var = String
type Name = String

type NEnv a = [(Name,a)]
type VEnv a = [(Var,a)]

data Exp = EBinOp Exp Op Exp
         | EUnOp Op Exp
         | EInt Int
         | EBool Bool
         | EString String
         | EVar Var
         | EUnit
         | EPrint Exp
         | EName Name

data Val = VInt Int
         | VString String
         | VBool Bool -- not used for Enriched Pi
         | VUnit 
         | VName Name

liftNum2 :: (Int -> Int -> Int) -> Val -> Val -> Val
liftNum2 f (VInt i) (VInt j) = VInt $ f i j
liftNum2 _ _ _ = error "type error"

liftNumBool2 :: (Int -> Int -> Bool) -> Val -> Val -> Val
liftNumBool2 f (VInt i) (VInt j) = VBool $ f i j
liftNumBool2 _ _ _ = error "type error"

liftStr2 :: (String -> String -> String) -> Val -> Val -> Val
liftStr2 f (VString s) (VString s') = VString $ f s s'
liftStr2 _ _ _ = error "type error"

binOpTable :: [(String,Val -> Val -> Val)]
binOpTable = [("+",liftNum2 (+)),
              ("-",liftNum2 (-)),
              ("<",liftNumBool2 (<)),
              ("*",liftStr2 (++))]

liftNum :: (Int -> Int) -> Val -> Val
liftNum f (VInt i) = VInt $ f i
liftNum _ _ = error "type error"

unOpTable :: [(String,Val -> Val)]
unOpTable = [("inv",liftNum $ \x -> -x)]


ppVal :: Val -> Doc
ppVal (VInt i) = int i
ppVal (VString s) = quotes $ text s
ppVal (VBool True) = text "true"
ppVal (VBool False) = text "false"
ppVal VUnit = lparen <> rparen
ppVal (VName n) = text "@" <> text n

instance Show Val where
    show = render . ppVal

ppExp :: Exp -> Doc
ppExp (EBinOp e1 op e2) = parens $ ppExp e1 <+> text op <+> ppExp e2
ppExp (EUnOp op e) = parens $ text op <+> ppExp e
ppExp (EInt i) = int i
ppExp (EBool True) = text "true"
ppExp (EBool False) = text "false"
ppExp (EVar v) = text v
ppExp (EString s) = quotes $ text s
ppExp EUnit = lparen <> rparen
ppExp (EPrint e) = text "print" <> parens (ppExp e) 
ppExp (EName n) = text "@" <> text n

instance Show Exp where
    show = render . ppExp

parseExp = tries [parsePrint,
                  parseString,
                  parseName,
                  parseInt,
                  parseBool,
                  parseVar,
                  parseBin,
                  parseUn,
                  parseUnit]

parseBin = paren $ do
             e1 <- parseExp
             op <- lexeme (oneOf "+-*<")
             e2 <- parseExp
             return $ EBinOp e1 [op] e2
parseUn = paren $ do
            op <- symbol "inv"
            e <- parseExp
            return $ EUnOp op e
parseInt = fmap (EInt . read) num
parseBool = parseTrue <|> parseFalse
    where parseTrue = symbol "true" >> return (EBool True)
          parseFalse = symbol "false" >> return (EBool False)
parseString = fmap EString $ between (symbol "\"") (symbol "\"") (many1 (alphaNumChar <|> spaceChar))
parseVar = fmap EVar name
parseUnit = symbol "()" >> return EUnit
parseName = do
  string "@"
  n <- name
  return $ EName n
parsePrint = do
  symbol "print"
  e <- paren $ parseExp
  return $ EPrint e
