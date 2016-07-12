module Language.Joy.Evaluator where

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
