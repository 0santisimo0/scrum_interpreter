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


parseComparison :: Parser Comparison
parseComparison = Comp <$> parseExpression <*> parseCompOperator <*> parseExpression

parseCompOperator :: Parser CompOperator
parseCompOperator = (Equal <$ string "==")
                  <|>(NotEqual <$ string "/=")
                  <|>(Less <$ string "<")
                  <|>(LessEqual <$ string "<=")
                  <|>(Greater <$ string ">")
                  <|>(GreaterEqual <$ string ">=")

{-
parseConditional :: Parser Expression
parseConditional = do
  _ <- string "if" *> spaces *> char '('
  condition <- parseComparison
  _ <- char ')' *> spaces *> char '{'
  ifExpr <- parseExpression
  _ <- char '}' *> spaces *> string "else" *> spaces *> char '{'
  elseExpr <- parseExpression
  _ <- char '}'
  return $ Conditional condition ifExpr elseExpr
-}
parseConditional :: Parser Expression
parseConditional =
    string "if" *> spaces *> char '(' *>
    parseComparison >>= \condition ->
    char ')' *> spaces *> char '{' *>
    parseExpression >>= \ifExpr ->
    char '}' *> spaces *> string "else" *> spaces *> char '{' *>
    parseExpression >>= \elseExpr ->
    char '}' *> 
    pure (Conditional condition ifExpr elseExpr)