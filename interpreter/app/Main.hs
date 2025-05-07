module Main where

import Lexer
import Parser
import Interpreter
import System.Environment (getArgs)
import Data.Char (isSpace)

main :: IO ()
main = do
  args <- getArgs
  
  if null args
    then putStrLn "Usage: interpreter <query-file>"
    else do
      let fileName = head args
      content <- readFile fileName
      let tokens = alexScanTokens content
      let ast = parse tokens
      interpret ast