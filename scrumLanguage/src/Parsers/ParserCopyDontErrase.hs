module Parsers.ParserCopyDontErrase (parseProgram) where

import AST as AST
import Text.Parsec
import Text.Parsec.String


parseIdentifier :: Parser Identifier
parseIdentifier = many1 letter


parseNumberLiteral :: Parser Literal
parseNumberLiteral = try parseAsFloatingPoint <|> parseAsInteger
  where
    parseAsFloatingPoint = do
      intPart <- many1 digit
      fracPart <- many1 digit
      return $ FloatingPointLiteral (read (intPart ++ "." ++ fracPart))

    parseAsInteger = do
      intPart <- many1 digit
      return $ IntegerLiteral (read intPart)


stringChain :: Parser String
stringChain = char '"' *> many (noneOf "\"") <* char '"'


parseLiteral :: Parser Literal
parseLiteral = try parseBoolLiteral <|> parseNumberLiteral <|> parseStringLiteral
  where
    parseBoolLiteral = (BooleanLiteral True <$ string "true") <|> (BooleanLiteral False <$ string "false")
    parseStringLiteral = StringLiteral <$> stringChain


parseRoleExp :: Parser Role
parseRoleExp = parseRoleWithPrefix '>' ["SM", "PO", "TM"]


parseRoleWithPrefix :: Char -> [String] -> Parser Role
parseRoleWithPrefix prefix roleNames = do
  _ <- char prefix
  roleName <- choice (map string roleNames)
  spaces
  case roleName of
    "SM" -> ScrumMaster <$> stringChain
    "PO" -> ProductOwner <$> stringChain
    "TM" -> TeamMember <$> stringChain
    _    -> undefined


parseAssignSymbol :: Parser ()
parseAssignSymbol = spaces <* string ":=" <* spaces


parseExpression :: Parser Expression
parseExpression = try parseFunctionCall
              <|> try parseLiteralExpression
              <|> try parseRole
              <|> try parseAssign
              <|> try parseFunction
              <|> try parseConditional
              <|> try parseReturn
              <|> parseVariable
  where
    parseAssign = Assign <$> (parseIdentifier <* parseAssignSymbol) <*> (Literal <$> parseLiteral)
    parseLiteralExpression = Literal <$> parseLiteral
    parseRole = Role <$> parseRoleExp
    parseFunctionCall = do
        _ <- char ':'
        funcName <- parseIdentifier
        spaces
        args <- between (char '(') (char ')') (parseExpression `sepBy` (char ',' >> spaces))
        return $ FunctionCall funcName args


parseComparison :: Parser Comparison
parseComparison = Comp <$> parseExpression <*> parseCompOperator <*> parseExpression


parseCompOperator :: Parser CompOperator
parseCompOperator = (Equal <$ string "==")
                  <|>(NotEqual <$ string "/=")
                  <|>(Less <$ string "<")
                  <|>(LessEqual <$ string "<=")
                  <|>(Greater <$ string ">")
                  <|>(GreaterEqual <$ string ">=")


parseReturn :: Parser Expression
parseReturn = ReturnStatement <$> (string "return" *> spaces *> parseExpression)

parseVariable :: Parser Expression
parseVariable = Variable <$> many1 letter


parseConditional :: Parser Expression
parseConditional = do
  _ <- string "if" *> spaces *> char '('
  condition <- parseComparison
  _ <- char ')' *> spaces *> char '{' *> spaces
  ifExpr <- parseMultipleExpressions
  _ <- spaces *> string "else" *> spaces *> char '{' *> spaces
  Conditional condition ifExpr <$> parseMultipleExpressions


parseFunction :: Parser Expression
parseFunction = do
  _ <- string "fun" *> spaces
  funcName <- many1 letter
  _ <- spaces *> char '('
  params <- sepBy1 (many1 letter) (spaces *> char ',' <* spaces)
  _ <- char ')' *> spaces *> char '{' *> spaces
  Function funcName params <$> parseMultipleExpressions


parseMultipleExpressions :: Parser [Expression]
parseMultipleExpressions = sepEndBy parseExpression (many1 (space <|> newline)) <* char '}'


parseProgram :: Parser [Expression]
parseProgram = sepBy parseExpression (many1 space <|> many1 newline)