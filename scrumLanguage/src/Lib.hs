module Lib( someFunc ) where

import Text.Parsec
import Parser as P


someFunc :: IO ()
someFunc = do
    content <- readFile "./resources/code.qs"
    let result = parse (P.parseLiteral) "./resources/code.qs" content
    print result
