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
    ( dot
    , dup
    )
    where

import Language.Joy.AST
import Language.Joy.State

data JoyError = InvalidState String

type JoyResult = IO (Either JoyError State)

failWith :: Monad m => a -> m (Either a b)
failWith = pure . Left

succeedWith :: b -> IO (Either a b)
succeedWith = pure . Right

dup :: State -> JoyResult
dup (State input (x:xs) env) = succeedWith (State input (x:x:xs) env)
dup _ = failWith (InvalidState "Invalid state for operation dup")

dot :: State -> JoyResult
dot s = do
  putStrLn (show s)
  return (pure s)
