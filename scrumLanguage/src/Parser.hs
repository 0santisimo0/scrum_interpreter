module Parser(parseExpression) where

import AST as AST
import Text.Parsec
import Text.Parsec.String



parseExpression :: Parser Expression
parseExpression = Assign <$> (many1 letter <* char ':' <* char '=' ) <*> (Value . read <$> many1 digit)


