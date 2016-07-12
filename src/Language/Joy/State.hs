{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module      : Language.Joy.Interpreter
-- Copyright   : (c) 2016 Owain Lewis
--
-- License     : BSD-style
-- Maintainer  : owain@owainlewis.com
-- Stability   : experimental
-- Portability : GHC
--
-- A Joy language interpreter
--
module Language.Joy.State where

import           Control.Lens     (makeLenses, over, view)
import qualified Data.Map         as M
import           Language.Joy.AST

type Env = M.Map String [Joy]

-- State represents a snapshot of the Joy runtime state
--
-- The input stack contains program instructions to process
-- The ouput stack contains the current program output state
-- The env contains a user defined list of instructions
--
data State = State {
    _input  :: [Joy]
  , _output :: [Joy]
  , _env    :: Env
} deriving ( Show, Eq )

makeLenses ''State

initialState :: [Joy] -> State
initialState program =
    State { _input = program, _output = [], _env = M.empty }

-- Modify state by adding a value to the environment
insertEnv :: State -> String -> [Joy] -> State
insertEnv state k v = over env (M.insert k v) state

pushStack :: Joy -> State -> State
pushStack v = over output (\xs -> v : xs)
