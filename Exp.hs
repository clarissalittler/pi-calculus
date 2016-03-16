module Exp where

import Text.PrettyPrint.HughesPJ
import ParserAux

import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

type Op = String
type Var = String
type Name = String

data Exp = EBinOp Exp Op Exp
         | EUnOp Op Exp
         | EInt Int
         | EBool Bool
         | EString String
         | EVar Var
         | EUnit
         | EPrint Exp
         | EName Name

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
