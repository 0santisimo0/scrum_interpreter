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
parseBinaryExpression =
    (try (FloatingPointLiteral <$> parseFloat) <|> (IntegerLiteral <$> parseInteger)) >>= \leftValue ->
    parseBinaryOperator >>= \operator ->
    (try (FloatingPointLiteral <$> parseFloat) <|> (IntegerLiteral <$> parseInteger)) >>= \rightValue ->
    return $ BinaryExpression (BinExpr leftValue operator rightValue)