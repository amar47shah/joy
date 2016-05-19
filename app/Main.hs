module Main where

import           Language.Joy.Core as Joy

repl :: IO a
repl = do
  input <- getLine
  case (Joy.runJoy input) of
    Right value -> putStrLn (show value)
    Left e -> putStrLn . show $ e
  repl

main :: IO ()
main = putStrLn "OK"
