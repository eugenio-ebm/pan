module Main where

import System.IO (hFlush, stdout)

import Control.Monad (forever)

import Pan.Expr (Expr)

import Pan.Scanner (scan)
import Pan.Parser (parse)
import Pan.Typechecker (typecheck)
import Pan.Evaluator (eval)

evalFile :: String -> IO ()
evalFile filepath = do
  s <- readFile filepath
  v <- eval . parse . scan $ s
  print v

typecheckFile :: String -> IO ()
typecheckFile filepath = do
  s <- readFile filepath
  let t = typecheck . parse . scan $ s
  print t

main :: IO ()
main = forever $ do
  putStr "> "
  hFlush stdout
  getLine >>= eval . parse . scan
