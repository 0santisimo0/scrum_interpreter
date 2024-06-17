module Lib( someFunc ) where

import Text.Parsec
import Parser as P

someFunc :: IO ()
someFunc = do
    literals <- readFile "./resources/Literals.qs"
    let literalsResults = parse (P.parseProgram) "./resources/Literals.qs" literals
    print "Literals Parse"
    print literalsResults

    variableAssign <- readFile "./resources/VariableAssign.qs"
    let variableResults = parse (P.parseProgram) "./resources/VariableAssign.qs" variableAssign
    print "VariableAssign Parse"
    print variableResults

    binaryOperator <- readFile "./resources/BinaryOperator.qs"
    let binaryOperatorResults = parse (P.parseProgram) "./resources/BinaryOperator.qs" binaryOperator
    print "BinaryOperator Parse"
    print binaryOperatorResults

    forLoop <- readFile "./resources/ForLoop.qs"
    let forLoopResults = parse (P.parseProgram) "./resources/ForLoop.qs" forLoop
    print "ForLoop Parse"
    print forLoopResults
