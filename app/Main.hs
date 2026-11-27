module Main where

import System.IO (hFlush, stdout)

import Control.Monad (forever)

import Pan.Expr (Expr)

import Pan.Scanner (scan)
import Pan.Parser (parse)

main :: IO ()
main = forever $ do
  putStr "> "
  hFlush stdout
  getLine >>= eval . parse . scan
