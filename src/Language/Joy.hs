module Language.Joy where

import           Control.Applicative    ((<$>))
import           Language.Joy.Evaluator (runRecursive)
import           Language.Joy.Parser    (parseJoy)
import           Language.Joy.State     (liftState)
import           Language.Joy.State     (State)

-- | Run a single joy program passed as a string
--
runProgram :: String -> IO ()
runProgram input = do
    case (liftState <$> parseJoy input) of
      Left e -> print ("Failed to parse program: " ++ show e) >> return ()
      Right state -> do
        evaluation <- runRecursive (pure state) 0
        case evaluation of
          Left e -> print ("Failed " ++ show e) >> return ()
          Right s -> return ()
