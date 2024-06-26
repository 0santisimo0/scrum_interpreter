module Lib (someFunc) where

-- import Text.Parsec
-- import Parsers.Parser (parseProgram)
-- import CodeGenerator (generateCode)
-- import System.IO (writeFile)

-- someFunc :: IO ()
-- someFunc = do
--     -- literals <- readFile "./resources/Literals.qs"
--     -- let literalsResults = parse (P.parseProgram) "./resources/Literals.qs" literals
--     -- print "Literals Parse"
--     -- print literalsResults

--     -- variableAssign <- readFile "./resources/VariableAssign.qs"
--     -- let variableResults = parse (P.parseProgram) "./resources/VariableAssign.qs" variableAssign
--     -- print "VariableAssign Parse"
--     -- print variableResults

--     -- binaryOperator <- readFile "./resources/BinaryOperator.qs"
--     -- let binaryOperatorResults = parse (P.parseProgram) "./resources/BinaryOperator.qs" binaryOperator
--     -- print "BinaryOperator Parse"
--     -- print binaryOperatorResults

--     forLoop <- readFile "./resources/code.qs"
--     let forLoopResults = parse parseProgram "./resources/code.qs" forLoop
--     print "Parser: "
--     print forLoopResults


import Parsers.Parser (parseProgram, ParserState)
import AST
import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Monad.Identity (Identity)
import qualified Data.Map as Map
import Control.Monad.State
import CodeGenerator

someFunc :: IO ()
someFunc = do
    -- Lee el contenido completo del archivo
    completeExample <- readFile "./resources/pythonAccepted.qs"
    
    -- Intenta parsear el contenido del archivo usando parsec
    let parseResult = runParser parseProgram initialState "pythonAccepted.qs" completeExample
    
    -- Maneja el resultado del parsing
    case parseResult of
        Left err -> putStrLn $ "Error de parsing: " ++ show err
        -- Left err -> do 
        --     print err
        Right expressions -> do
            print expressions
            let pythonCode = generateCode expressions
            writeFile "src/PythonFiles/test.py" pythonCode
            putStrLn "Generated test.py"


-- Estado inicial del parser (en este caso, una tabla de símbolos vacía)
initialState :: ParserState
initialState = Map.empty