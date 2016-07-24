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
import qualified Data.Map as M

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

findOperation :: String -> Maybe (State -> JoyResult)
findOperation k = M.lookup k allOperations
    where allOperations = M.fromList [ ("first", _first)
                                     , ("rest", _rest)
                                     , ("size", _size)
                                     , ("i", _i)
                                     , ("swap", _swap)
                                     , ("dup", _dup)
                                     , (".", _dot)
                                     , ("+", _plus)
                                     , ("-", _minus)
                                     ]

type JoyResult = IO (Either JoyError State)

failWith :: Monad m => a -> m (Either a b)
failWith = pure  .Left

succeedWith :: b -> IO (Either a b)
succeedWith = pure . Right

failFor :: Monad m => String -> m (Either JoyError b)
failFor op = failWith $ InvalidState ("Invalid state for operation " <> op)

_plus :: State -> JoyResult
_plus (State (JoySymbol("+"):input) ((JoyNumber x):(JoyNumber y):xs) env) =
    succeedWith $ State input (JoyNumber(x+y):xs) env
_plus _ = failFor "+"

_minus :: State -> JoyResult
_minus (State (JoySymbol("-"):input) ((JoyNumber x):(JoyNumber y):xs) env) =
    succeedWith $ State input (JoyNumber(x-y):xs) env
_minus _ = failFor "-"

-- | Pushes an extra copy of X onto stack
-- | X -> X X
_dup :: State -> JoyResult
_dup (State (JoySymbol("dup"):input) (x:xs) env) =
    succeedWith $ State input (x:x:xs) env
_dup _ = failFor "dup"

-- | Dot is used to print the current state of the output stack
-- |
_dot :: State -> JoyResult
_dot state@(State (JoySymbol("."):input) output env) = do
    print . show . _output $ state
    return . pure $ State input output env
_dot _ = failFor "."

-- | Interchanges X and Y on top of the stack
-- | X Y -> Y X
_swap :: State -> JoyResult
_swap (State (JoySymbol("swap"):input) (x:y:xs) env) =
  let ouput = y:x:xs in
      succeedWith $ State input ouput env
_swap _ = failFor "swap"

-- | Executes P. So, [P] i  ==  P
-- | [P]  -> ...
_i :: State -> JoyResult
_i state@(State (JoySymbol("i"):input) (JoyQuote vs:xs) env) =
    succeedWith $ State (vs++input) xs env
_i _ = failFor "i"

-- | F is the first member of the non-empty aggregate A
-- | A -> F
_first :: State -> JoyResult
_first (State (JoySymbol("first"):input) (JoyQuote(y:ys):xs) env) =
    succeedWith $ State input (y:xs) env
_first _ = failFor "first"

-- | R is the non-empty aggregate A with its first member removed
-- | A -> R
_rest :: State -> JoyResult
_rest (State (JoySymbol("rest"):input) (JoyQuote(y:ys):xs) env) =
    succeedWith $ State input (JoyQuote(ys):xs) env
_rest _ = failFor "rest"

-- | Integer I is the number of elements of aggregate A
-- | A  ->  I
_size :: State -> JoyResult
_size (State (JoySymbol("size"):input) (JoyQuote(qs):xs) env ) =
    succeedWith $ State input (JoyNumber(length qs):xs) env
_size _ = failFor "size"

_ifte (State (JoySymbol("ifte"):input) (p:t:e:xs) env) = failFor "Not sure how to implement :("
