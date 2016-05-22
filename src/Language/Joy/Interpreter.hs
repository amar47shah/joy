module Language.Joy.Interpreter
       ( Stack
       , pop
       , push
       , peek
       ) where

import           Control.Exception.Base (Exception, throw)
import           Control.Monad.State
import           Language.Joy.Parser    (Joy (..))

data InterpeterException =
    InvalidStateException String
    deriving (Show)

instance Exception InterpeterException

type Stack = [Joy]

pop ::  StateT Stack IO Joy
pop = do
    stack <- get
    case stack of
      [] -> liftIO . throw  $ InvalidStateException "Empty stack"
      (x:xs) -> do
        put xs
        return x

pop2 ::  StateT Stack IO (Joy, Joy)
pop2 = do
    stack <- get
    case stack of
      (x:y:xs) -> do
          put xs
          return (x,y)
      _ -> liftIO . throw $ InvalidStateException "Expected two elements on stack"

push :: Joy -> StateT Stack IO ()
push v = do
    stack <- get
    put (v:stack)
    return ()

peek :: StateT Stack IO (Maybe Joy)
peek = do
    stack <- get
    case stack of
      [] -> return Nothing
      (x:xs) -> return $ Just x

-- Swap the top two elements on the stack
swap :: StateT Stack IO ()
swap = do
    stack <- get
    case stack of
      (x:y:xs) -> do
        put (y:x:xs)
        return ()
      _ -> liftIO . throw $ InvalidStateException "Expected two elements on the stack"

program = do
    push (JoyNumber 10)
    push (JoyNumber 2)
    pop

example = runStateT program []
