module Lib( someFunc ) where

import Text.Parsec
import Parser as P
import BinaryExpressionParser as PB


someFunc :: IO ()
someFunc = do
    content <- readFile "./resources/code.qs"
    let result = parse (P.parseProgram) "./resources/code.qs" content
    print result
