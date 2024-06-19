module Parsers.Parser (
    parseProgram
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
  , P.reservedNames   = ["if", "else", "True", "False", "for", "fun", "return", "in", "SM", "PO", "TM", "US"]
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
parseBoolean = (True <$ reserved "True")  <|> (False <$ reserved "False")

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

-- commaSep :: Parser a -> Parser [a]
-- commaSep = P.commaSep lexer


parseLiteral :: Parser Literal
parseLiteral = try (FloatingPointLiteral <$> parseFloat)
      <|> (IntegerLiteral <$> parseInteger)
      <|> (BooleanLiteral <$> parseBoolean)
      <|> (StringLiteral <$> parseStringLiteral)


parseVariable :: Parser Expression
parseVariable = Variable <$> parseIdentifier


parseAssign :: Parser Expression
parseAssign = Assign
    <$> parseIdentifier
    <*> (reservedOp ":=" *> parseExpression)


parseBinaryOperator :: Parser BinaryOperator
parseBinaryOperator = 
  (Add <$ reservedOp "+")
  <|> (Sub <$ reservedOp "-")
  <|> (Mul <$ reservedOp "*")
  <|> (Div <$ reservedOp "/")


parseBinaryExpression :: Parser Expression
parseBinaryExpression = 
    try (BinaryExpression <$> 
        (BinExprLit <$> 
            (try (FloatingPointLiteral <$> parseFloat) 
            <|> try (IntegerLiteral <$> parseInteger))
        <*> parseBinaryOperator 
        <*> (try (FloatingPointLiteral <$> parseFloat) 
            <|> try (IntegerLiteral <$> parseInteger))))
    <|> try (BinaryExpression <$>
        (BinExprId <$>
            parseIdentifier
        <*> parseBinaryOperator
        <*> parseIdentifier))

parseElement :: Parser Literal
parseElement = parseLiteral


sameType :: [Literal] -> Bool
sameType [] = True
sameType (x:xs) = all ((== getType x) . getType) xs
  where
    getType :: Literal -> String
    getType (BooleanLiteral _) = "Boolean"
    getType (IntegerLiteral _) = "Integer"
    getType (FloatingPointLiteral _) = "Float"
    getType (StringLiteral _) = "String"


parseListExpression :: Parser Expression
parseListExpression = 
  (ListExpr <$> 
    parseIdentifier <* 
    reservedOp "<" <*> 
    (parseElement `sepBy` reservedOp ",") <* 
    reservedOp ">"
  ) >>= \listExpr ->
  if sameType (getElements listExpr)
    then return (ListExpression listExpr)
    else fail "All elements in the list must be of the same type"
  where
    getElements (ListExpr _ elems) = elems

parseIterable :: Parser Expression
parseIterable = try parseListExpression <|> parseVariable


parseForLoop :: Parser Expression
parseForLoop = 
  reserved "for" *> 
  parens ((,) <$> parseAssign <*> (reserved "in" *> parseIterable)) >>= \(var, iterable) ->
  ForLoopExpression <$> (ForLoop var iterable <$> braces parseExpression)


parseExpression :: Parser Expression
parseExpression = try parseFunction
              <|> try parseBinaryExpression
              <|> try parseForLoop
              <|> try parseListExpression
              <|> try parseLiteralExpression
              <|> try parseUserStory
              <|> try parseRole
              <|> try parseAssign
              <|> try parseConditional
              <|> try parseReturn
              <|> try parseFunctionCall
              <|> parseVariable
  where
    parseLiteralExpression = Literal <$> parseLiteral
    parseRole = Role <$> parseRoleExp


parseFunctionCall :: Parser Expression
parseFunctionCall =
    FunctionCall
    <$> (char ':' *> parseIdentifier)
    <*> (spaces *> between (char '(') (char ')') (parseExpression `sepBy` (char ',' >> spaces)))



parseRoleExp :: Parser Role
parseRoleExp = (reserved "SM" *> spaces *> char ':'  *> spaces >> ScrumMaster <$> parseStringLiteral)
    <|> (reserved "PO"  *> spaces *> char ':'  *> spaces >> ProductOwner <$> parseStringLiteral)
    <|> (reserved "TM"  *> spaces *> char ':'  *> spaces >> TeamMember <$> parseStringLiteral)


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
parseReturn = ReturnStatement <$> (reserved "return" *> spaces *> parseExpression)


parseConditional :: Parser Expression
parseConditional = 
  reserved "if" *> spaces *> char '(' *> parseComparison <* char ')' <* spaces <* char '{' <* spaces >>= \condition ->
  parseMultipleExpressions <* spaces <* string "else" <* spaces <* char '{' <* spaces >>= \ifExpr ->
  Conditional condition ifExpr <$> parseMultipleExpressions


parseFunction :: Parser Expression
parseFunction = 
  reserved "fun" *> spaces *> parseIdentifier >>= \funcName ->
  char '(' *> sepBy1 (many1 letter) (spaces *> char ',' <* spaces) <* char ')' <* spaces <* char '{' <* spaces >>= \params ->
  Function funcName params <$> parseMultipleExpressions


whiteSpace :: Parser ()
whiteSpace = P.whiteSpace lexer

parseMultipleExpressions :: Parser [Expression]
parseMultipleExpressions = manyTill (parseExpression <* whiteSpace) (try (whiteSpace *> char '}'))


parseUserStoryType :: Parser UserStoryType
parseUserStoryType = 
  (Feature <$ reserved "Feature")
  <|> (Spike <$ reserved "Spike")
  <|> (POC <$ reserved "POC") 
  <|> (Fix <$  reserved "Fix")
  <|> (HotFix <$ reserved "HotFix")


parseUserStoryFormatBlock :: Parser UserStoryFormatBlock
parseUserStoryFormatBlock = UserStoryFormatBlock
    <$> (reserved "T" *> char ':' *> whiteSpace *> parseStringLiteral <* char ',' <* whiteSpace)
    <*> (reserved "TY" *> char ':' *>  whiteSpace *> parseUserStoryType <* char ',' <* whiteSpace)
    <*> (reserved "PS" *> char ':' *> whiteSpace *> char '(' *>   parseRoleExp <* char ')' <* char ','<* whiteSpace)
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
parseProgram :: Parser [Expression]
parseProgram = whiteSpace *> parseExpression `endBy` void (many $ oneOf "\n\r") <* eof