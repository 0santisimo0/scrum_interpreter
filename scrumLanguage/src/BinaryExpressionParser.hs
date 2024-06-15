module BinaryExpressionParser (parseBinaryExpression) where

import AST
import Parser
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Language
import qualified Text.Parsec.Token

parseBinaryOperator :: Parser BinaryOperator
parseBinaryOperator = (reservedOp "+" >> return Add)
            <|> (reservedOp "-" >> return Sub)
            <|> (reservedOp "*" >> return Mul)
            <|> (reservedOp "/" >> return Div)

parseBinaryExpression :: Parser Expression
parseBinaryExpression = do
    left <-  try (FloatingPointLiteral <$> parseFloat) <|> (IntegerLiteral <$> parseInteger)
    op <- parseBinaryOperator
    right <- try (FloatingPointLiteral <$> parseFloat) <|> (IntegerLiteral <$> parseInteger)
    return $ BinaryExpression (IntBinExpr left op right)