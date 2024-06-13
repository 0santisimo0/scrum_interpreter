module Parser (parseProgram) where

import AST as AST
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token
import Text.Parsec.Language


languageDef :: LanguageDef st
languageDef = emptyDef
  { commentStart    = "/*"
  , commentEnd      = "*/"
  , commentLine     = "//"
  , nestedComments  = True
  , identStart      = letter
  , identLetter     = alphaNum <|> oneOf "_'"
  , opStart         = opLetter emptyDef
  , opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , reservedNames   = ["BEGIN", "END"]
  , reservedOpNames = ["+", "-", "*", "/", ":=", "==", "<", ">", "<=", ">="]
  , caseSensitive   = True
  }


lexer:: TokenParser st
lexer = makeTokenParser languageDef


int:: Parser Integer
int = Text.Parsec.Token.integer lexer


parens :: Parser a -> Parser a
parens= Text.Parsec.Token.parens lexer

whiteSpaces = Text.Parsec.Token.whiteSpace lexer

reservedOps :: String -> Parser()
reservedOps = Text.Parsec.Token.reservedOp lexer

-- reservedNmaes :: String -> Parser()
-- reservedNames 

plusSign = whiteSpaces <* reservedOps "+" *> whiteSpaces


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
parseExpression = try parseFunctionCall <|> parseLiteralExpression <|> parseRole <|> parseAssign
  where
    parseAssign = Assign <$> (parseIdentifier <* parseAssignSymbol) <*> (Literal <$> parseLiteral)
    parseLiteralExpression = Literal <$> parseLiteral
    parseRole = Role <$> parseRoleExp
    parseFunctionCall = do
        funcName <- parseIdentifier
        spaces
        args <- between (char '(') (char ')') (parseExpression `sepBy` (char ',' >> spaces))
        return $ FunctionCall funcName args


parseProgram :: Parser [Expression]
parseProgram = sepBy parseExpression (many1 space <|> many1 newline)