module Main where

import Parser
import Typechecker

main :: IO ()
main =
    process "mapfold"
    --line <- getLine
    --process line
  where process = print .
                  typecheck .
                  addPrelude .
                  parse
