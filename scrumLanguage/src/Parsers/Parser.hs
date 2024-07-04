{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Parsers.Parser (
    parseProgram, ParserState
    ) where

import AST
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Language
import qualified Text.Parsec.Token as P
import Control.Monad (void)
import Data.List (intercalate)
import Control.Monad.State
import qualified Data.Map as Map
import Data.Functor.Identity (Identity)
import Data.Bifunctor

type SymbolTable = Map.Map String Expression
type ContextStack = [SymbolTable]
type Errors = [String]
type ParserState = (ContextStack, Errors)
type MyParser = ParsecT String ParserState Identity

initialState :: ParserState
initialState = ([Map.empty], [])

updateSymbolTable :: String -> Expression -> MyParser ()
updateSymbolTable var val = modifyCurrentContext (Map.insert var val)

addError :: String -> MyParser ()
addError err = modifyState (second (err :))

hasErrors :: MyParser Bool
hasErrors = not . null . snd <$> getState


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

parseIdentifier :: MyParser String
parseIdentifier = P.identifier lexer

parseInteger :: MyParser Integer
parseInteger = P.integer lexer

parseFloat :: MyParser Double
parseFloat = P.float lexer

parseBoolean :: MyParser Bool
parseBoolean = (True <$ reserved "True")  <|> (False <$ reserved "False")

parseStringLiteral :: MyParser String
parseStringLiteral = P.stringLiteral lexer

reserved :: String -> MyParser ()
reserved = P.reserved lexer

reservedOp :: String -> MyParser ()
reservedOp = P.reservedOp lexer

parens :: MyParser a -> MyParser a
parens = P.parens lexer

braces :: MyParser a -> MyParser a
braces = P.braces lexer


variableExists :: String -> MyParser Bool
variableExists var = do
  (ctxs, _) <- getState
  case ctxs of
    [] -> return False
    (currentContext:_) -> return $ Map.member var currentContext


variableExistsInAnyContext :: String -> MyParser Bool
variableExistsInAnyContext var = do
  (ctxs, _) <- getState
  return $ any (Map.member var) ctxs


varExistsInGlobalContext :: String -> MyParser Bool
varExistsInGlobalContext var = do
  (ctxs, _) <- getState
  case reverse ctxs of
    [] -> return False
    (globalContext:_) -> return $ Map.member var globalContext


pushContext :: MyParser ()
pushContext = modifyState (\(ctx:ctxs, errs) -> (Map.empty : ctx : ctxs, errs))

popContext :: MyParser ()
popContext = modifyState (\(_:ctx:ctxs, errs) -> (ctx : ctxs, errs))

modifyCurrentContext :: (SymbolTable -> SymbolTable) -> MyParser ()
modifyCurrentContext f = modifyState (\(ctx:ctxs, errs) -> (f ctx : ctxs, errs))


parseLiteral :: MyParser Literal
parseLiteral = try (FloatingPointLiteral <$> parseFloat)
      <|> (IntegerLiteral <$> parseInteger)
      <|> (BooleanLiteral <$> parseBoolean)
      <|> (StringLiteral <$> parseStringLiteral)

parseVariable :: MyParser Expression
parseVariable = do
    pos <- getPosition
    var <- parseIdentifier
    exists <- variableExists var
    if exists
      then return (Variable var)
      else do
        let line = sourceLine pos
        let column = sourceColumn pos
        error ("Variable " ++ var ++ " no definido (" ++ show line ++ ", "++ show column ++")")

parseParam :: MyParser Parameter
parseParam = do
    pos <- getPosition
    var <- parseIdentifier
    existsInGlobal <- varExistsInGlobalContext var
    if existsInGlobal
      then do
        (ctxs, errs) <- getState
        let globalContext = last ctxs
        let val = globalContext Map.! var
        modifyCurrentContext (Map.insert var val)
        return (Parameter var)
      else do
        let line = sourceLine pos
        let column = sourceColumn pos
        error ("Parametro " ++ var ++ " no esta definido en el contexto global (" ++ show line ++ ", "++ show column ++")")


parseAssign :: MyParser Expression
parseAssign = do
    pos <- getPosition
    var <- parseIdentifier
    reservedOp ":="
    exists <- variableExists var
    if exists
        then do
            let line = sourceLine pos
            let column = sourceColumn pos
            let errorMsg = "Variable " ++ var ++ " ya existe (" ++ show line ++ ", " ++ show column ++ ")"
            addError errorMsg
            error errorMsg
        else getAssignParser var

getAssignParser :: Identifier -> MyParser Expression
getAssignParser var = do
    val <- parseExpression
    updateSymbolTable var val
    pure (Assign var val)


parseBinaryOperator :: MyParser BinaryOperator
parseBinaryOperator =
  (Add <$ reservedOp "+")
  <|> (Sub <$ reservedOp "-")
  <|> (Mul <$ reservedOp "*")
  <|> (Div <$ reservedOp "/")

parseBinaryExpression :: MyParser Expression
parseBinaryExpression = do
  lhs <- parseTerm
  op <- parseBinaryOperator
  rhs <- parseTerm
  return $ case (lhs, rhs) of
    (Left lit1, Left lit2) -> BinaryExpression (BinExprLit lit1 op lit2)
    (Right var1, Right var2) -> BinaryExpression (BinExprId var1 op var2)
    _ -> error "Both sides of the binary expression must be either literals or identifiers"
  where
    parseTerm :: MyParser (Either Literal Identifier)
    parseTerm = 
          (Left <$> parseLiteral)
      <|> (Right <$> parseVarIdentifier)

    parseVarIdentifier :: MyParser Identifier
    parseVarIdentifier = do
      pos <- getPosition
      var <- parseIdentifier
      exists <- variableExists var
      if exists
        then return var
        else do
          let line = sourceLine pos
          let column = sourceColumn pos
          let errorMsg = "Variable " ++ var ++ " no definida (" ++ show line ++ ", " ++ show column ++ ")"
          addError errorMsg
          error errorMsg


parseElement :: MyParser Literal
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

parseListExpression :: MyParser Expression
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

parseIterable :: MyParser Expression
parseIterable = try parseListExpression <|> parseVariable


parseForLoop :: MyParser Expression
parseForLoop = do
  reserved "for"
  (varAssign, iterable) <- parens $ do
    varAssign <- parseAssign
    reserved "in"
    iterable <- parseIterable
    return (varAssign, iterable)
  body <- braces parseMultipleExpressions
  return $ ForLoopExpression (ForLoop varAssign iterable body)

parseExpression :: MyParser Expression
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

parseFunctionCall :: MyParser Expression
parseFunctionCall = do
  _ <- char ':'
  pos <- getPosition
  funcName <- parseIdentifier
  exists <- variableExists funcName
  if not exists
    then do
      let line = sourceLine pos
      let column = sourceColumn pos
      error ("Funcion " ++ funcName ++ " no existe (" ++ show line ++ ", " ++ show column ++ ")")
    else FunctionCall funcName <$> (spaces *> between (char '(') (char ')') (parseExpression `sepBy` (char ',' >> spaces)))

parseRoleExp :: MyParser Role
parseRoleExp = (reserved "SM" *> spaces *> char ':'  *> spaces >> ScrumMaster <$> parseStringLiteral)
    <|> (reserved "PO"  *> spaces *> char ':'  *> spaces >> ProductOwner <$> parseStringLiteral)
    <|> (reserved "TM"  *> spaces *> char ':'  *> spaces >> TeamMember <$> parseStringLiteral)

parseComparison :: MyParser Comparison
parseComparison = Comp <$> parseExpression <*> parseCompOperator <*> parseExpression

parseCompOperator :: MyParser CompOperator
parseCompOperator = try (Equal <$ string "==")
                  <|> try (LessEqual <$ string "<=")
                  <|> try (GreaterEqual <$ string ">=")
                  <|> try (NotEqual <$ string "/=")
                  <|> try (Less <$ string "<")
                  <|> try (Greater <$ string ">")

parseReturn :: MyParser Expression
parseReturn = ReturnStatement <$> (reserved "return" *> spaces *> parseExpression)

parseConditional :: MyParser Expression
parseConditional = do
  reserved "if"
  spaces
  char '('
  condition <- parseComparison
  char ')'
  spaces
  char '{'
  spaces
  ifExpr <- parseMultipleExpressions
  spaces
  string "else"
  spaces
  char '{'
  spaces
  elseExpr <- parseMultipleExpressions
  return $ Conditional condition ifExpr elseExpr



parseFunction :: MyParser Expression
parseFunction = do
  reserved "fun"
  spaces
  pos <- getPosition
  funcName <- parseIdentifier
  exists <- variableExistsInAnyContext funcName
  if exists
    then do
      let line = sourceLine pos
      let column = sourceColumn pos
      error ("Function " ++ funcName ++ " ya existe (" ++ show line ++ ", " ++ show column ++ ")")
    else do
      pushContext
      params <- char '(' *> sepBy1 parseParam (spaces *> char ',' <* spaces) <* char ')'
      spaces *> char '{' *> spaces
      body <- parseMultipleExpressions
      popContext
      let func = Function funcName params body
      updateSymbolTable funcName func
      return func

whiteSpace :: MyParser ()
whiteSpace = P.whiteSpace lexer

parseMultipleExpressions :: MyParser [Expression]
parseMultipleExpressions = manyTill (parseExpression <* whiteSpace) (try (whiteSpace *> char '}'))

parseUserStoryType :: MyParser UserStoryType
parseUserStoryType =
  (Feature <$ reserved "Feature")
  <|> (Spike <$ reserved "Spike")
  <|> (POC <$ reserved "POC")
  <|> (Fix <$  reserved "Fix")
  <|> (HotFix <$ reserved "HotFix")

parseUserStoryFormatBlock :: MyParser UserStoryFormatBlock
parseUserStoryFormatBlock = UserStoryFormatBlock
    <$> (reserved "T" *> char ':' *> whiteSpace *> parseStringLiteral <* char ',' <* whiteSpace)
    <*> (reserved "TY" *> char ':' *>  whiteSpace *> parseUserStoryType <* char ',' <* whiteSpace)
    <*> (reserved "PS" *> char ':' *> whiteSpace *> char '(' *>   parseRoleExp <* char ')' <* char ','<* whiteSpace)
    <*> (reserved "DS" *> char ':' *>  whiteSpace *> parseStringLiteral <* char ',' <* whiteSpace)
    <*> (reserved "ET" *> char ':' *>  whiteSpace *> parseInteger<* char ',' <* whiteSpace)
    <*> (reserved "AC" *> char ':' *>  whiteSpace *> parseStringLiteral)

parseUserStory :: MyParser Expression
parseUserStory =
    UserStory <$> ( reserved "US" *>
        ( UserStoryExpr
        <$>parseStringLiteral
        <*> (char '{' *> whiteSpace *>  parseUserStoryFormatBlock <* whiteSpace <* char '}'
        )))


parseProgram :: MyParser [Expression]
parseProgram = whiteSpace *> many (parseExpression <* whiteSpace) <* eof