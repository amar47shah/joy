module Language.Joy.Interpreter
       ( Stack
       , pop
       , push
       , peek
       ) where

import           Control.Exception.Base (Exception, throw)
import           Control.Monad.State
import qualified Data.Map               as M
import           Language.Joy.Parser    (Joy (..))

data InterpeterException =
    InvalidStateException String
    deriving (Show)

instance Exception InterpeterException

type Stack = [Joy]

data Interpreter = Interpreter {
    stack :: [Joy]                   -- The runtime stack (current program state)
  , env   :: M.Map String [Joy]      -- The global environment defined by user
} deriving ( Show )

-------------------------------------------------
-- Core stack operations
-------------------------------------------------

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

-------------------------------------------------
-- Native
-------------------------------------------------

dot :: StateT Stack IO ()
dot = do
    stack <- get
    case stack of
      (x:xs) -> liftIO . print . show $ x
      [] -> return ()

-- Runs a binary operation on the stack
binOp :: (Integer -> Integer -> Integer) -> StateT Stack IO ()
binOp op = do
    stack <- get
    case stack of
      (JoyNumber x : JoyNumber y : xs) -> do
          put (JoyNumber (x `op` y) : xs)
          return ()
      _ -> liftIO . throw $ InvalidStateException "Expected two numbers on the stack"

-------------------------------------------------

-------------------------------------------------
-- Stateful evaluation
-------------------------------------------------

eval :: [Joy] -> StateT Stack IO ()
eval [] = return ()
eval ((JoyNumber n) : xs) = push (JoyNumber n) >> eval xs
eval ((JoyLiteral ".") : xs) = dot >> eval xs
eval ((JoyLiteral "+") : xs) = binOp (+) >> eval xs

-------------------------------------------------

eg = [JoyNumber 1, JoyNumber 2, JoyLiteral "+", JoyLiteral "."]

exec :: [Joy] -> IO ()
exec program = runStateT (eval program) [] >> return ()
