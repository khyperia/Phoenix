module Main where

import Parser
import Typechecker
import Evaluator

main :: IO ()
main = do
    line <- getLine
    putStrLn $ process line
  where process = evalTypecheck .
                  addPrelude .
                  parse
        evalTypecheck x = typecheck x `seq` evaluate x
