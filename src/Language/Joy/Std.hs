-- |
-- Module      : Language.Joy.Std
-- Copyright   : (c) 2016 Owain Lewis
--
-- License     : BSD-style
-- Maintainer  : owain@owainlewis.com
-- Stability   : experimental
-- Portability : GHC
--
-- Standard library for Joy including all primary combinators
--
module Language.Joy.Std
    ( allOperations
    , findOperation
    , dot
    , dup
    ) where

import Language.Joy.AST
import Language.Joy.State
import Data.Map as M
import Data.Monoid((<>))

-- | The following table of operations represent the primary
-- | constructs that form the Joy programming language.
allOperations :: Map String (State -> JoyResult)
allOperations = M.fromList $ Prelude.foldl1 (++) [combinators, io, math, operators]

operators :: [(String, (State -> JoyResult))]
operators = [ ("first", first)
            , ("rest", rest)
            ]

combinators :: [(String, (State -> JoyResult))]
combinators = [ ("i", i)
              , ("swap", swap)
              , ("dup", dup)
              ]

io :: [(String, (State -> JoyResult))]
io = [ (".", dot) ]

math :: [(String, (State -> JoyResult))]
math = [ ("+", plus)
       , ("-", plus)
       ]

findOperation :: String -> Maybe (State -> JoyResult)
findOperation k = M.lookup k allOperations

type JoyResult = IO (Either JoyError State)

failWith :: Monad m => a -> m (Either a b)
failWith = pure . Left

succeedWith :: b -> IO (Either a b)
succeedWith = pure . Right

-- | OPERATORS

-- id Identity function, does nothing.\nAny program of the form  P id Q  is equivalent to just  P Q.
-- dup Pushes an extra copy of X onto stack. X  ->   X X"
-- Swap Interchanges X and Y on top of the stack. X Y  ->   Y X
-- Rollup Moves X and Y up, moves Z down X Y Z  ->  Z X Y
-- Rolldown Moves Y and Z down, moves X up X Y Z  ->  Y Z X
-- Rotate Interchanges X and Z X Y Z  ->  Z Y X

failFor :: Monad m => String -> m (Either JoyError b)
failFor op = failWith $ InvalidState ("Invalid state for operation " <> op)

-- | Basic addition
plus :: State -> JoyResult
plus (State (JoySymbol("+"):ys) ((JoyNumber x):(JoyNumber y):xs) env) =
    succeedWith $ State ys (JoyNumber(x+y):xs) env
plus _ = failFor "+"

-- | Basic subtraction
minus :: State -> JoyResult
minus (State (JoySymbol("-"):ys) ((JoyNumber x):(JoyNumber y):xs) env) =
    succeedWith $ State ys (JoyNumber(x-y):xs) env
minus _ = failFor "-"

-- | Duplicates the item on the top of the stack
dup :: State -> JoyResult
dup (State (JoySymbol("dup"):ys) (x:xs) env) =
    succeedWith $ State ys (x:x:xs) env
dup _ = failFor "dup"

-- | Dot is used to print the current state of the run stack
dot :: State -> JoyResult
dot state@(State (JoySymbol("."):ys) output env) = do
    print . show . _output $ state
    return . pure $ State ys output env
dot _ = failFor "."

-- | Swap simply swaps the top two items on the stack
swap (State (JoySymbol("swap"):ys) (x:y:xs) env) =
  let newIn = ys
      newOut = y:x:xs in
      succeedWith $ State newIn newOut env
swap _ = failFor "swap"

-- | Executes P. So, [P] i  ==  P
-- | [P]  ->  ...
i :: State -> JoyResult
i state@(State (JoySymbol("i"):ys) (JoyQuote vs:xs) env) =
    succeedWith $ State (vs++ys) xs env
i _ = failFor "i"

-- | F is the first member of the non-empty aggregate A
-- | A -> F
first (State (JoySymbol("first"):input) (JoyQuote(y:ys):xs) env) =
    succeedWith $ State input (y:xs) env
first _ = failFor "first"

-- | R is the non-empty aggregate A with its first member removed
-- | A -> R
rest (State (JoySymbol("rest"):input) (JoyQuote(y:ys):xs) env) =
    succeedWith $ State input (JoyQuote(ys):xs) env
rest _ = failFor "rest"
