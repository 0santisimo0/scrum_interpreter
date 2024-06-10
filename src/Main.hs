module Main where

import UU.Parsing
import Scanner
import AbstractGrammar
import Parser
import CodeGenerator

main :: IO ()
main = do 
    input <- readFile "text.jef"
    let token = scanner input
    print token
    -- tree <- parseIO pVariableLiteral token
    -- print tree
