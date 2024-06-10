module Parser where

 
import UU.Parsing
import Scanner
import AbstractGrammar



-- pExpression = VariableLiteral <$ pKeyword ":" <*> pStrings

pVariableLiteral :: Parser Token Expression
pVariableLiteral = VariableLiteral <$> (pKeyword ":" *> pStrings) <*> pLiteral

-- Parser para literales
pLiteral :: Parser Token Literal
pLiteral = pBoolLiteral <|> pIntLiteral <|> pFloatLiteral <|> pStringLiteral
  where
    pBoolLiteral = BooleanLiteral <$> pBool
    pIntLiteral = IntegerLiteral <$> pNatural
    pFloatLiteral = FloatingPointLiteral <$> pDouble
    pStringLiteral = StringLiteral <$> pStrings


instance Symbol Token

getValue:: Token -> String
getValue (Token _ v _ _) = v
 
tSym :: Type -> String -> Parser Token String
tSym typ value = getValue <$> pSym (Token typ value 0 0)
 
tStr = getValue <$> pSym (Token String "" 0 0)
 
pKeyword :: String -> Parser Token String
pKeyword = tSym Keyword

pStrings :: Parser Token String
pStrings = tStr

-- Parser para literales booleanos
pBool :: Parser Token Bool
pBool = (pSym (Token Boolean "true" 0 0) *> pure True) <|> (pSym (Token Boolean "false" 0 0) *> pure False)

-- Parser para literales enteros
pNatural :: Parser Token Integer
pNatural = read <$> tSym Integer ""


-- Parser para literales de punto flotante
pDouble :: Parser Token Double
pDouble = read <$> tSym Double ""



-- parseProgram = pExpression <*> pList pExpression

-- pExpression = pLiteral <|> pVariable <|> pFunctionCall <|> pConditional

-- pLiteral = Literal <$> pLiteral'

-- pLiteral' = pBooleanLiteral <|> pIntegerLiteral <|> pFloatingPointLiteral <|> pStringLiteral

-- pBooleanLiteral = BooleanLiteral <$> pBool

-- pBool = pTrue <|> pFalse
--   where
--     pTrue = token "true" *> pure True
--     pFalse = token "false" *> pure False

-- pIntegerLiteral = IntegerLiteral <$> pNatural

-- pFloatingPointLiteral = FloatingPointLiteral <$> pDouble

-- pStringLiteral = StringLiteral <$> pStringLit

-- pVariable = Variable <$> pIdent

-- pFunctionCall = FunctionCall <$> pIdent <*> (parenthesized (commaSep pExpression) <|> pure [])

-- pConditional = Conditional <$> (token "if" *> pBoolExpression)
--                             <*> (token "then" *> pExpression)
--                             <*> (token "else" *> pExpression)

-- pBoolExpression = undefined -- Define el parser para BoolExpression

-- Funciones auxiliares para el análisis sintáctico

-- parenthesized p = token "(" *> p <* token ")"

-- pIdent = lexeme $ (:) <$> letter <*> many alphaNum

-- pEnd = pSpaces

-- pSpaces = skipMany (satisfy isSpace)