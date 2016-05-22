module Main where

import           Language.Joy.Interpreter as Interp

runFile :: FilePath -> IO ()
runFile f = do
    contents <- readFile f
    Interp.run contents

repl :: IO a
repl = do
  input <- getLine
  Interp.run input
  repl

main :: IO ()
main = print "OK"
