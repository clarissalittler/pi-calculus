module Exp where

import Text.PrettyPrint.HughesPJ
import ParserAux

import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

type Op = String
type Var = String

data Exp = EBinOp Exp Op Exp
         | EUnOp Op Exp
         | EInt Int
         | EBool Bool
         | EString String
         | EVar Var
         | EUnit
         | EPrint Exp

ppExp :: Exp -> Doc
ppExp (EBinOp e1 op e2) = parens $ ppExp e1 <+> text op <+> ppExp e2
ppExp (EUnOp op e) = parens $ text op <+> ppExp e
ppExp (EInt i) = int i
ppExp (EBool True) = text "true"
ppExp (EBool False) = text "false"
ppExp (EVar v) = text v
ppExp (EString s) = quotes $ text s
ppExp EUnit = lparen <> rparen

instance Show Exp where
    show = render . ppExp

parseExp = tries [parseBin,
                  parseUn,
                  parseInt,
                  parseBool,
                  parseString,
                  parseVar,
                  parseUnit,
                  parsePrint]

parseBin = paren $ do
             e1 <- parseExp
             op <- many1 asciiChar
             e2 <- parseExp
             return $ EBinOp e1 op e2
parseUn = paren $ do
            op <- many1 asciiChar
            e <- parseExp
            return $ EUnOp op e
parseInt = fmap (EInt . read) num
parseBool = parseTrue <|> parseFalse
    where parseTrue = symbol "true" >> return (EBool True)
          parseFalse = symbol "false" >> return (EBool False)
parseString = fmap EString $ between (symbol "\"") (symbol "\"") (many1 asciiChar)
parseVar = fmap EVar name
parseUnit = symbol "()" >> return EUnit
parsePrint = do
  symbol "print"
  e <- paren $ parseExp
  return $ EPrint e

