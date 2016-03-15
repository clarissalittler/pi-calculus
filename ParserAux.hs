module ParserAux where

import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

spacey :: Parser ()
spacey = L.space (spaceChar >> return ()) (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")
lexeme = L.lexeme spacey
symbol = L.symbol spacey

many1 p = do
  v <- p
  vs <- many p
  return $ v : vs

name = lexeme (many1 letterChar)
num = lexeme (many1 digitChar)
paren = between (symbol "(") (symbol ")")
bracey = between (symbol "{") (symbol "}")
dot = symbol "."
commaSep x = sepBy1 x (symbol ",")

tries :: [Parser a] -> Parser a
tries = choice . map try
