module Parser (parseProgram) where

import AST
import Text.Parsec
import Text.Parsec.String


parseIdentifier :: Parser Identifier
parseIdentifier = many1 letter



parseLiteral :: Parser Literal
parseLiteral = parseBoolLiteral <|> parseIntLiteral <|> parseFloatLiteral <|> parseStringLiteral
  where
    parseBoolLiteral = (BooleanLiteral True <$ string "true") <|> (BooleanLiteral False <$ string "false")
    parseIntLiteral = IntegerLiteral . read <$> many1 digit
    parseFloatLiteral = FloatingPointLiteral . read <$> ((++) <$> many1 digit <*> ((:) <$> char '.' <*> many1 digit))
    parseStringLiteral = StringLiteral <$> (char '"' *> many (noneOf "\"") <* char '"')



parseExpression :: Parser Expression
parseExpression = try parseAssign <|> parseLiteralExpression
  where
    parseAssign = Assign <$> (parseIdentifier <* spaces <* string ":=" <* spaces) <*> (Literal <$> parseLiteral)
    parseLiteralExpression = Literal <$> parseLiteral



parseProgram :: Parser [Expression]
parseProgram = sepBy parseExpression (many1 space <|> many1 newline)

parserComparison :: Parser Comparison
parserComparison = Comp <$> parseExpression <*> parserCompOperator <*> parseExpression

parserCompOperator :: Parser CompOperator
parserCompOperator = (Equal <$ string "==")
                  <|>(NotEqual <$ string "/=")
                  <|>(Less <$ string "<")
                  <|>(LessEqual <$ string "<=")
                  <|>(Greater <$ string ">")
                  <|>(GreaterEqual <$ string ">=")