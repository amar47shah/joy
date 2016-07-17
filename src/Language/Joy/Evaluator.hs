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

import           Data.Monoid        ((<>))
import           Language.Joy.AST
import           Language.Joy.State
import           Language.Joy.Std   (findOperation)

returnState :: Applicative f => [Joy] -> [Joy] -> Env -> f (Either a State)
returnState i o e = pure . Right $ State i o e

-- | Run some state exactly once
run :: State -> IO (Either JoyError State)
-- Terminal clause. There is no more input to process
run state@(State [] _ _) =
    return . pure $ state
-- Push onto output stack
run state@(State (i@(JoyNumber _):xs) output env) =
    returnState xs (i:output) env
-- Push onto output stack
run state@(State (i@(JoyString _):xs) output env) =
    returnState xs (i:output) env
-- Push onto output stack
run state@(State (i@(JoyBool _):xs) output env) =
    returnState xs (i:output) env
-- Push onto output stack
run state@(State (i@(JoyQuote _):xs) output env) =
    returnState xs (i:output) env
-- Lookup symbol for native match else search user env and apply all values to stack
run state@(State (i@(JoySymbol sym):xs) output env) =
    case (findOperation sym) of
        Just f -> f state
        Nothing ->
          case (getEnv sym state) of
              -- Push all operations back onto the stack and then evaluate
              Just ops -> return . Right $ State (ops ++ xs) output env
              -- A symbol was passed that doesn't exist as a native operation or a user defined function
              Nothing -> return . Left $ RuntimeError ("Unbound symbol " ++ sym)
run state@(State input output env) = return $ Left (RuntimeError "Unsupported terminal clause")

debug :: Show a => IO a -> IO ()
debug x = x >>= print . show

-- | Recursively evaluate the input program but stop if we hit an error
-- |
-- | @
-- | λ> let s = initialState [JoyNumber 10, JoyNumber 20, JoyNumber 30]
-- | λ> runRecursive (pure s) 0
-- | @
runRecursive :: IO State -> Int -> IO (Either JoyError State)
runRecursive state step = do
    innerState <- state
    -- print $ "Running step " <> (show step) <> " " <> show innerState
    result <- run innerState
    case result of
      Right (newState@(State input output env)) ->
        if (null input) then
          return . Right $ newState
        else
            do
              runRecursive (pure newState) (step+1)
      Left e -> return . Left $ e
