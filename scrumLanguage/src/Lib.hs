module Lib (someFunc) where

import Text.Parsec
import Parsers.Parser as P
import CodeGenerator (generateCode)
import System.IO (writeFile)

someFunc :: IO ()
someFunc = do
    -- literals <- readFile "./resources/Literals.qs"
    -- let literalsResults = parse (P.parseProgram) "./resources/Literals.qs" literals
    -- print "Literals Parse"
    -- print literalsResults

    -- variableAssign <- readFile "./resources/VariableAssign.qs"
    -- let variableResults = parse (P.parseProgram) "./resources/VariableAssign.qs" variableAssign
    -- print "VariableAssign Parse"
    -- print variableResults

    -- binaryOperator <- readFile "./resources/BinaryOperator.qs"
    -- let binaryOperatorResults = parse (P.parseProgram) "./resources/BinaryOperator.qs" binaryOperator
    -- print "BinaryOperator Parse"
    -- print binaryOperatorResults

    -- forLoop <- readFile "./resources/ForLoop.qs"
    -- let forLoopResults = parse (P.parseProgram) "./resources/ForLoop.qs" forLoop
    -- print "ForLoop Parse"
    -- print forLoopResults

    completeExample <- readFile "./resources/pythonAccepted.qs"
    let parseResult = parse (P.parseProgram) "./resources/pythonAccepted.qs" completeExample
    putStrLn "COMPLETE EXAMPLE"
    print parseResult
    case parseResult of
        Left err -> print err
        Right ast -> do
            let pythonCode = generateCode ast
            writeFile "src/test.py" pythonCode
            putStrLn "Generated test.py"
