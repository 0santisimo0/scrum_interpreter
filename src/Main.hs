module Main where

import UU.Parsing
import Scanner
import AbstractGrammar
import Parser
import CodeGenerator

main :: IO ()
main = do 
    input <- readFile "text.jef"
    putStrLn input
    putStrLn "hola mundo"
