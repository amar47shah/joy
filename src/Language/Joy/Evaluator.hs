-- |
-- Module      : Language.Joy.Parser
-- Copyright   : (c) 2016 Owain Lewis
--
-- License     : BSD-style
-- Maintainer  : owain@owainlewis.com
-- Stability   : experimental
-- Portability : GHC
--
-- The Joy evaulator which takes a program state and recursively evaluates
--
module Language.Joy.Evaluator
    ( run
    , runRecursive
    ) where

import Language.Joy.AST
import Language.Joy.State
import Data.Monoid((<>))

returnState :: Applicative f => [Joy] -> [Joy] -> Env -> f (Either a State)
returnState i o e = pure . Right $ State i o e

-- | Run some state exactly once
--
run :: State -> IO (Either JoyError State)
run state@(State [] _ _) =
    pure . pure $ state
run state@(State (i@(JoyNumber _):xs) output env) =
    returnState xs (i:output) env
run state@(State (i@(JoyString _):xs) output env) =
    returnState xs (i:output) env
run state@(State (i@(JoyBool _):xs) output env) =
    returnState xs (i:output) env
run state@(State (i@(JoyQuote _):xs) output env) =
    returnState xs (i:output) env

debug :: Show a => IO a -> IO ()
debug x = x >>= print . show

-- | Recursively evaluate the input program but stop if we hit an error
--
-- @
-- λ> let s = initialState [JoyNumber 10, JoyNumber 20, JoyNumber 30]
-- λ> runRecursive (pure s) 0
-- @
--
runRecursive :: IO State -> Int -> IO (Either JoyError State)
runRecursive state step = do
    innerState <- state
    print $ "Running step " <> (show step) <> " " <> show innerState
    result <- run innerState
    case result of
      Right (newState@(State input output env)) ->
        if (null input) then
          return . Right $ newState
        else
          do
            runRecursive (pure newState) (step+1)
      Left e -> return . Left $ e
