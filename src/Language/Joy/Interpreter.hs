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

-------------------------------------------------
-- Errors
-------------------------------------------------

data InterpeterException =
    InvalidStateException String
    deriving (Show)

instance Exception InterpeterException

stateException :: MonadIO m => String -> m a
stateException msg = ioThrow (InvalidStateException msg)
    where ioThrow = liftIO . throw

-------------------------------------------------

type Stack = [Joy]

data Interpreter = Interpreter {
    -- Stack runtime state
    stack :: Stack
    -- Global environment that can be modified by user
  , env   :: M.Map String [Joy]
} deriving ( Eq, Show )

setEnv :: String -> [Joy] -> Interpreter -> Interpreter
setEnv k v (Interpreter stack env) = Interpreter stack newEnv
    where newEnv = M.insert k v env

getEnv :: String -> Interpreter -> Maybe [Joy]
getEnv k (Interpreter stack env) = M.lookup k env

setEnvState :: String -> [Joy] -> StateT Interpreter IO ()
setEnvState k v = modify (setEnv k v) >> return ()

-------------------------------------------------
-- Debugging
-------------------------------------------------

checkEnv :: StateT Interpreter IO ()
checkEnv = do
    (Interpreter _ e) <- get
    liftIO . print . show $ e
    return ()

-------------------------------------------------
-- Core stack operations
-------------------------------------------------

pop ::  StateT Interpreter IO Joy
pop = do
    (Interpreter stack env) <- get
    case stack of
      [] -> stateException "Empty stack"
      (x:xs) -> do
        put (Interpreter xs env)
        return x

push :: Joy -> StateT Interpreter IO ()
push v = do
   (Interpreter stack env) <- get
   put (Interpreter (v:stack) env)
   return ()

peek :: StateT Interpreter IO (Maybe Joy)
peek = do
    interp <- get
    case (stack interp) of
      [] -> return Nothing
      (x:xs) -> return $ Just x

-- Swap the top two elements on the stack
swap :: StateT Interpreter IO ()
swap = do
    (Interpreter stack env) <- get
    case stack of
      (x:y:xs) -> do
         put (Interpreter (y:x:xs) env)
         return ()
      _ -> stateException "Expected two elements on the stack"

-------------------------------------------------
-- Native
-------------------------------------------------

dot :: StateT Interpreter IO ()
dot = do
    interp <- get
    case (stack interp) of
      (x:xs) -> let f = liftIO . print . show in f x
      [] -> return ()

-- Runs a binary operation on the stack
binOp :: (Integer -> Integer -> Integer) -> StateT Interpreter IO ()
binOp op = do
    (Interpreter stack env) <- get
    case stack of
      (JoyNumber x : JoyNumber y : xs) -> do
          let ns = JoyNumber (x `op` y) : xs
          put (Interpreter ns env)
          return ()
      _ -> stateException "Expected two numbers on the stack"

-------------------------------------------------
-- Combinators
-------------------------------------------------

-------------------------------------------------
-- Stateful evaluation
-------------------------------------------------

eval :: [Joy] -> StateT Interpreter IO ()
eval [] = return ()
eval ((JoyNumber x) : xs) = push (JoyNumber x) >> eval xs
eval ((JoyString x) : xs) = push (JoyString x) >> eval xs
eval ((JoyAssignment k v) : xs) = setEnvState k v >> eval xs
eval ((JoyComment _) : xs) = eval xs
eval ((JoyLiteral ".") : xs) = dot >> eval xs
eval ((JoyLiteral "+") : xs) = binOp (+) >> eval xs
eval ((JoyLiteral "-") : xs) = binOp (-) >> eval xs
eval ((JoyLiteral "*") : xs) = binOp (*) >> eval xs

-------------------------------------------------

eg = [JoyNumber 1, JoyNumber 2, JoyLiteral "+", JoyLiteral "."]

eg2 = [JoyAssignment "square" [JoyLiteral "dup", JoyLiteral "*"], JoyLiteral "."]

exec :: [Joy] -> IO ()
exec program = runStateT (eval program >> checkEnv) (Interpreter [] M.empty) >> return ()
