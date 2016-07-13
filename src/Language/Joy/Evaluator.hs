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
    ) where

import Language.Joy.AST
import Language.Joy.State

returnState :: Monad m => [Joy] -> [Joy] -> Env -> m State
returnState i o e = return $ State i o e

run :: State -> IO State
run state@(State [] _ _) = return state
run state@(State (i@(JoyNumber _):xs) output env) = returnState xs (i:output) env
run state@(State (i@(JoyString _):xs) output env) = returnState xs (i:output) env
run state@(State (i@(JoyBool _):xs) output env) = returnState xs (i:output) env
run state@(State (i@(JoyQuote _):xs) output env) = returnState xs (i:output) env
