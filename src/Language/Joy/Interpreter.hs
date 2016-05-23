module Language.Joy.Interpreter
       ( Stack
       , pop
       , push
       , peek
       , exec
       , run
       ) where

import           Control.Exception.Base (Exception, throw)
import           Control.Monad.State
import qualified Data.Map               as M
import           Language.Joy.Parser    (Joy (..), parseJoy)

reservedCombinators :: [String]
reservedCombinators = [ "swap"
                      , "cons"
                      , "dup"
                      , "zap"
                      ]

-------------------------------------------------
-- Errors
-------------------------------------------------

data InterpeterException =
    InvalidStateException String
    | ArgumentException String
    | RuntimeException String
    deriving (Show)

instance Exception InterpeterException

debug :: (Show r, MonadIO m) => r -> m ()
debug x = let f = liftIO . print . show in f x

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
      (x:xs) -> return $ Just x
      [] -> return Nothing

-------------------------------------------------
-- Native
-------------------------------------------------

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

-- Print the first element on the stack
dot :: StateT Interpreter IO ()
dot = do
    interp <- get
    case (stack interp) of
      (x:xs) -> debug x
      [] -> return ()

-- Swap the top two elements on the stack
swap :: StateT Interpreter IO ()
swap = do
    (Interpreter stack env) <- get
    case stack of
      (x:y:xs) -> do
         put (Interpreter (y:x:xs) env)
         return ()
      _ -> stateException "Expected two elements on the stack"

-- Duplicate the top element on the stack
dup :: StateT Interpreter IO ()
dup = do
    (Interpreter stack env) <- get
    case stack of
      (x:xs) -> put (Interpreter (x:x:xs) env) >> return ()
      _ -> stateException "Empty stack"

-- Pop the first element off the stack
zap :: StateT Interpreter IO Joy
zap = do
    (Interpreter stack env) <- get
    case stack of
      (x:xs) -> put (Interpreter xs env) >> return x
      _ -> stateException "Empty stack"

-------------------------------------------------
-- Combinators
-------------------------------------------------

-- i, dip, cons

cons :: StateT Interpreter IO ()
cons = do
  (Interpreter stack env) <- get
  case stack of
    ((JoyQuote ys) : x : xs) -> do
       put (Interpreter ((JoyQuote (x:ys)) : xs) env)
       return ()
    _ -> stateException "Invalid arguments"

unit :: StateT Interpreter IO ()
unit = do
    (Interpreter stack env) <- get
    case stack of
      (x:xs) -> do
          put (Interpreter ((JoyQuote [x]) : xs) env)
          return ()
      _ -> stateException "Empty stack"

-- The I combinator is a partial one that breaks the patterns. I need to pass the
-- runtime stack in to Interp to do this properly. Basically returns elements
-- that get `re-added` to the runtime and THEN evaluated
i :: StateT Interpreter IO [Joy]
i = do
    (Interpreter stack env) <- get
    case stack of
      ((JoyQuote q) : xs) -> do
          put (Interpreter xs env)
          return q
      _ -> stateException "Invalid arguments"

-------------------------------------------------
-- Stateful evaluation
-------------------------------------------------

eval :: [Joy] -> StateT Interpreter IO ()
-- Inductive case
eval [] = return ()
--- Core types (mainly push operations)
eval ((JoyNumber x) : xs)       = push (JoyNumber x) >> eval xs
eval ((JoyString x) : xs)       = push (JoyString x) >> eval xs
eval ((JoyAssignment k v) : xs) = setEnvState k v >> eval xs
eval ((JoyQuote x) : xs)        = push (JoyQuote x) >> eval xs
eval ((JoyComment _) : xs)      = eval xs
--- Combinators and native features
eval ((JoyLiteral "dup") : xs)  = dup >> eval xs
eval ((JoyLiteral "cons") : xs) = cons >> eval xs
eval ((JoyLiteral "swap") : xs) = swap >> eval xs
eval ((JoyLiteral "zap") : xs)  = zap >> eval xs
eval ((JoyLiteral "unit") : xs) = unit >> eval xs
eval ((JoyLiteral "i") : xs)    = do
    r <- i
    eval (r++xs)
eval ((JoyLiteral ".") : xs)    = dot >> eval xs
eval ((JoyLiteral "+") : xs)    = binOp (+) >> eval xs
eval ((JoyLiteral "-") : xs)    = binOp (-) >> eval xs
eval ((JoyLiteral "*") : xs)     = binOp (*) >> eval xs
eval (x:xs) = liftIO . throw $ RuntimeException ("Failed to match " ++ (show x))

-------------------------------------------------

exec :: [Joy] -> IO ()
exec program = runStateT (eval program) (Interpreter [] M.empty) >> return ()

run :: String -> IO ()
run program =
    case (parseJoy program) of
      Left e -> print "Failed to parse program"
      Right p -> exec p
