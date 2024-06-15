module Lib( someFunc ) where

import Text.Parsec
import Parser as P
import BinaryExpressionParser as PB
import RoleParser as PR


someFunc :: IO ()
someFunc = do
    content <- readFile "./resources/code.qs"
    let result = parse (PR.parseRole) "./resources/code.qs" content
    print result
