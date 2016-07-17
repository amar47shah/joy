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
allOperations :: Map [Char] (State -> JoyResult)
allOperations = fromList[ ("dup", dup)
                        , ("+", plus)
                        , ("-", plus)
                        , ("i", i)
                        , (".", dot)
                        ]

findOperation :: String -> Maybe (State -> JoyResult)
findOperation k = M.lookup k allOperations

type JoyResult = IO (Either JoyError State)

failWith :: Monad m => a -> m (Either a b)
failWith = pure . Left

succeedWith :: b -> IO (Either a b)
succeedWith = pure . Right

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
dup (State (JoySymbol("dup"):ys) (x:xs) env) = succeedWith $ State ys (x:x:xs) env
dup _ = failFor "dup"

-- | Dot is used to print the current state of the run stack
dot :: State -> JoyResult
dot state@(State (JoySymbol("."):ys) output env) = do
    print (show state)
    return . pure $ State ys output env
dot _ = failFor "."

-- | These combinators are special in that they dequote stack items
-- | The "i" combinator simply executes the top item on the stack.
-- | @
-- | [A] i == A
-- | @
i :: State -> JoyResult
i state@(State (JoySymbol("i"):ys) (JoyQuote vs:xs) env) = succeedWith $ State (vs++ys) xs env
i _ = failFor "i"
