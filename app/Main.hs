module Main ( main ) where

import           Language.Joy.Evaluator
import           Language.Joy.Parser
import           Language.Joy.State

readProgram = do
    contents <- readFile "programs/simple.joy"
    case parseJoy contents of
      Left e -> return ()
      Right p -> return ()

main = print "OK"
