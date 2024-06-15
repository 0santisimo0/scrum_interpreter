module Parser(parseLiteral) where

import AST
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Language
import qualified Text.Parsec.Token as P

languageDef :: LanguageDef st
languageDef = emptyDef
  { P.commentStart    = "/*"
  , P.commentEnd      = "*/"
  , P.commentLine     = "//"
  , P.nestedComments  = True
  , P.identStart      = letter
  , P.identLetter     = alphaNum <|> oneOf "_$"
  , P.opStart         = P.opLetter languageDef
  , P.opLetter        = oneOf ":=<>+-*/"
  , P.reservedNames   = ["if", "else", "True", "False", "for", "in", "SM", "PO", "TM", "US"]
  , P.reservedOpNames = [":=", "+", "-", "*", "/", "==", "/=", "<", ">", "<=", ">="]
  , P.caseSensitive   = True
  }

lexer :: P.TokenParser st
lexer = P.makeTokenParser languageDef

parseIdentifier :: Parser String
parseIdentifier = P.identifier lexer

parseInteger :: Parser Integer
parseInteger = P.integer lexer

parseFloat :: Parser Double
parseFloat = P.float lexer

parseBoolean :: Parser Bool
parseBoolean = (reserved "True" >> return True) <|> (reserved "False" >> return False)

parseStringLiteral :: Parser String
parseStringLiteral = P.stringLiteral lexer

reserved :: String -> Parser ()
reserved = P.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = P.reservedOp lexer

parens :: Parser a -> Parser a
parens = P.parens lexer

braces :: Parser a -> Parser a
braces = P.braces lexer

commaSep :: Parser a -> Parser [a]
commaSep = P.commaSep lexer

parseLiteral :: Parser Literal
parseLiteral = try (FloatingPointLiteral <$> parseFloat)
      <|> (IntegerLiteral . fromIntegral <$> parseInteger)
      <|> (BooleanLiteral <$> parseBoolean)
      <|> (StringLiteral <$> parseStringLiteral)