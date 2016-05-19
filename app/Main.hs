module Main where

import           Language.Joy.Core as Joy

runFile :: FilePath -> IO ()
runFile f = do
    contents <- readFile f
    let parsed = Joy.runJoy contents
    print $ show parsed

repl :: IO a
repl = do
  input <- getLine
  case (Joy.runJoy input) of
    Right value -> putStrLn (show value)
    Left e -> print . show $ e
  repl

main :: IO ()
main = print "OK"
