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

allOperations :: Map [Char] (State -> JoyResult)
allOperations = fromList[ ("dup", dup)
                        , (".", dot)
                        ]

findOperation :: String -> Maybe (State -> JoyResult)
findOperation k = M.lookup k allOperations

type JoyResult = IO (Either JoyError State)

failWith :: Monad m => a -> m (Either a b)
failWith = pure . Left

succeedWith :: b -> IO (Either a b)
succeedWith = pure . Right

-- | Is there a more elegant way to bind these and prevent terminal recursion?

dup :: State -> JoyResult
dup (State (JoySymbol("dup"):ys) (x:xs) env) = succeedWith updatedState
    where updatedState = State ys (x:x:xs) env
dup _ = failWith $ InvalidState "Invalid state for operation dup"

dot :: State -> JoyResult
dot state@(State (JoySymbol("."):ys) output env) = do
  putStrLn (show state)
  return . pure $ State ys output env
dot _ = failWith $ InvalidState "Invalid state for operation dup"
