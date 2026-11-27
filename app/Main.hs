module Main where

import System.IO (hFlush, stdout)

import Control.Monad (forever)

import Pan.Val (ppVal)

import Pan.Scanner (scan)
import Pan.Parser (parse)
import Pan.Eval (eval)

main :: IO ()
main = forever $ do
  putStr "> "
  hFlush stdout
  getLine >>= putStrLn . ppVal . eval . parse . scan
