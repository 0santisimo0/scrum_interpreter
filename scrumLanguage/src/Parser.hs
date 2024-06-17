module Parser (
    parseProgram,
    parseUserStory
    ) where

import AST
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Language
import qualified Text.Parsec.Token as P
import Control.Monad (void)
import Data.List (intercalate)

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
  , P.reservedNames   = ["if", "True", "False", "for", "in", "SM", "PO", "TM", "US",
                        "Feature", "Spike", "POC", "Fix", "HotFix",
                        "T", "TY", "PS", "DS", "ET", "AC"]
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

whiteSpace :: Parser ()
whiteSpace = P.whiteSpace lexer

parseLiteral :: Parser Literal
parseLiteral = try (FloatingPointLiteral <$> parseFloat)
      <|> (IntegerLiteral . fromIntegral <$> parseInteger)
      <|> (BooleanLiteral <$> parseBoolean)
      <|> (StringLiteral <$> parseStringLiteral)

parseVariable :: Parser Expression
parseVariable = Variable <$> parseIdentifier

parseAssign :: Parser Expression
parseAssign = Assign
    <$> parseIdentifier
    <*> (reservedOp ":=" *> parseExpression)

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

parseElement :: Parser Literal
parseElement = parseLiteral

sameType :: [Literal] -> Bool
sameType [] = True
sameType (x:xs) = all (== getType x) (map getType xs)
  where
    getType :: Literal -> String
    getType (BooleanLiteral _) = "Boolean"
    getType (IntegerLiteral _) = "Integer"
    getType (FloatingPointLiteral _) = "Float"
    getType (StringLiteral _) = "String"

parseListExpression :: Parser Expression
parseListExpression =
    parseIdentifier >>= \id ->
    (reservedOp "<" *> parseElement `sepBy` reservedOp "," <* reservedOp ">") >>= \elems ->
    if sameType elems
        then return $ ListExpression (ListExpr id elems)
        else fail "All elements in the list must be of the same type"

parseIterable :: Parser Expression
parseIterable = try parseListExpression <|> parseVariable

parseForLoop :: Parser Expression
parseForLoop = 
    reserved "for" *> 
    parens ((,) <$> parseAssign <*> (reserved "in" *> parseIterable)) >>= \(var, iterable) ->
    braces parseExpression >>= \body ->
    return $ ForLoopExpression (ForLoop var iterable body)

parseRole :: Parser Role
parseRole = (reserved "SM" *> spaces *> char ':'  *> spaces >> ScrumMaster <$> parseStringLiteral)
    <|> (reserved "PO"  *> spaces *> char ':'  *> spaces >> ProductOwner <$> parseStringLiteral)
    <|> (reserved "TM"  *> spaces *> char ':'  *> spaces >> TeamMember <$> parseStringLiteral)

parseUserStoryType :: Parser UserStoryType
parseUserStoryType =  try (reserved "Feature" *> (return Feature))
                    <|> try (reserved "Spike" *> (return Spike))
                    <|> try (reserved "POC" *> (return POC))
                    <|> try (reserved "Fix" *> (return Fix))
                    <|> try (reserved "HotFix" *> (return HotFix))

parseUserStoryFormatBlock :: Parser UserStoryFormatBlock
parseUserStoryFormatBlock = UserStoryFormatBlock
    <$> (reserved "T" *> char ':' *> whiteSpace *> parseStringLiteral <* char ',' <* whiteSpace)
    <*> (reserved "TY" *> char ':' *>  whiteSpace *> parseUserStoryType <* char ',' <* whiteSpace)
    <*> (reserved "PS" *> char ':' *> whiteSpace *> char '(' *>   parseRole <* char ')' <* char ','<* whiteSpace)
    <*> (reserved "DS" *> char ':' *>  whiteSpace *> parseStringLiteral <* char ',' <* whiteSpace)
    <*> (reserved "ET" *> char ':' *>  whiteSpace *> parseInteger<* char ',' <* whiteSpace)
    <*> (reserved "AC" *> char ':' *>  whiteSpace *> parseStringLiteral)

parseUserStory :: Parser Expression
parseUserStory =
    UserStory <$> ( reserved "US" *> 
        ( UserStoryExpr 
        <$>parseStringLiteral
        <*> (char '{' *> whiteSpace *>  parseUserStoryFormatBlock <* whiteSpace <* char '}'
        )))

parseExpression :: Parser Expression
parseExpression = try parseAssign
        <|> try parseForLoop
        <|> try parseListExpression
        <|> try parseBinaryExpression
        <|> (LiteralExpr <$> parseLiteral)
        <|> try parseVariable

parseProgram :: Parser [Expression]
parseProgram = whiteSpace *> parseExpression `endBy` (void $ many $ oneOf "\n\r") <* eof
