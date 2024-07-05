module Lib (someFunc) where
    
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

    usExample <- readFile "./resources/UserStory.qs"

    let parseResultUS = runParser parseProgram initialState "UserStory.qs" usExample

    case parseResultUS of
        Left err -> putStrLn $ "Error de parsing: " ++ show err
        Right expressions -> do
            print expressions
            let pythonCode = generateCode expressions
            writeFile "src/PythonFiles/testUS.py" pythonCode
            putStrLn "Generated testUS.py"

    completeExample <- readFile "./resources/pythonAccepted.qs"

    let parseResult = runParser parseProgram initialState "pythonAccepted.qs" completeExample

    case parseResult of
        Left err -> putStrLn $ "Error de parsing: " ++ show err
        Right expressions -> do
            print expressions
            let pythonCode = generateCode expressions
            writeFile "src/PythonFiles/test.py" pythonCode
            putStrLn "Generated test.py"


initialState :: ParserState
initialState = ([Map.empty], [])