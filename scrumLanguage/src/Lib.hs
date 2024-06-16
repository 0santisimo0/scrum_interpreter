module Lib( someFunc ) where

import Text.Parsec
import Parsers.Parser as P


someFunc :: IO ()
someFunc = do
    content <- readFile "./resources/code.qs"
    let result = parse (P.parseProgram) "./resources/code.qs" content
    print result
