module Language.Joy.Core where

import           Control.Applicative ((<$>), (<*>))
import qualified Data.Map            as M

import           Language.Joy.Parser (Joy (..), parseJoy, pprint)
-----------------------------------------
-- | Standard lib
-----------------------------------------

type JoyF = [Joy] -> Either JoyError [Joy]

data JoyError = InvalidState | RuntimeException String
    deriving ( Show, Eq )

push :: [a] -> a -> [a]
push xs v = v : xs

pop :: JoyF
pop (_:xs) = pure xs
pop [] = Left InvalidState

binOp :: (Integer -> Integer -> Integer) -> JoyF
binOp op (JoyNumber y: JoyNumber x:xs) = pure $ JoyNumber (x `op` y) : xs
binOp _ _ = Left InvalidState

add :: JoyF
add stack = binOp (+) stack

mult :: JoyF
mult stack = binOp (*) stack

sub :: JoyF
sub stack = binOp (-) stack

-----------------------------------------
-- | Prelude env
-----------------------------------------

prelude :: M.Map String JoyF
prelude =
    M.fromList [ ("+", add)
               , ("*", mult)
               ]
-----------------------------------------
-- | Compiler
-----------------------------------------

type RuntimeStack = [Joy]
type ProgramStack = [Joy]

showError :: JoyError -> a
showError = error . show

-- | Runtime evaluation
eval :: RuntimeStack -> ProgramStack -> M.Map String [Joy] -> [Joy]

eval s [] env = reverse s
-- Comments are skipped
eval stack (value@(JoyComment _) : xs) env =
    eval stack xs env
-- Booleans are just pushed onto the stack
eval stack (value@(JoyBool _) : xs) env =
    eval (value : stack) xs env
-- Numbers are just pushed onto the stack
eval stack (value@(JoyNumber _) : xs) env =
    eval (value : stack) xs env
-- Quotations are just pushed onto the stack
eval stack (value@(JoyQuote _) : xs) env =
    eval (value : stack) xs env
-- Assignment alerts the programming environment
eval stack (value@(JoyAssignment k f) : xs) env =
    eval stack xs (M.insert k f env)
eval stack (value@(JoyLiteral "swap") : xs) env =
    case stack of
      (x:y:ys) -> eval (y:x:ys) xs env
      _ -> showError $ InvalidState
-- Base combinators : i, dip, cons, dup, zap
-- The i combinator (takes a quoted program, applies it to the stack and then evals)
eval stack (value@(JoyLiteral "i") : xs) env =
    case stack of
      (JoyQuote program) : ys -> eval ys (program ++ xs) env
      _ -> showError $ InvalidState
-- Dip
-- [B] [A] dip == A [B]
-- The "dip" combinator executes the top item "A", but first it gets rid of the second item,
-- which is restored after the execution of "A" is complete
eval stack (value@(JoyLiteral "dip") : xs) env =
    case stack of
      ((JoyQuote p1) : p2 : rs) -> eval rs (p1 ++ [p2] ++ xs) env
      _ -> showError $ InvalidState
-- Cons
eval stack (value@(JoyLiteral "cons") : xs) env =
    case stack of
      ((JoyQuote qs):x:xs) -> eval (JoyQuote (x : qs) : xs) xs env
      _ -> showError $ InvalidState
-- Dup
eval stack (value@(JoyLiteral "dup") : xs) env =
    case stack of
       (y:ys) -> eval (y:y:ys) xs env
       _ -> showError $ InvalidState
-- Zap
eval stack (value@(JoyLiteral "zap") : xs) env =
    case stack of
      (y:ys) -> eval ys xs env
      _ -> showError $ InvalidState
eval stack (value@(JoyLiteral l) : xs) env =
    case (M.lookup l prelude) of
      -- Try the native env first
      Just f -> case (f stack) of
                  Left e ->
                     showError $ RuntimeException ("Failed to apply " ++ (show e))
                  Right s -> eval s xs env
      Nothing ->
          -- If native lookup fails then try user defined env
          case (M.lookup l env) of
                      Just p -> eval stack (p++xs) env
                      Nothing -> showError $ RuntimeException message
                          where message = "Unbound literal " ++ l ++ " " ++ (show stack)

-- | Runs a joy program by parsing a string and evaluating
--
runJoy :: String -> Either String [Joy]
runJoy input =
    case (parseJoy input) of
      Right program -> Right $ eval [] program M.empty
      Left e -> Left (show e)

runJoyPretty :: String -> Either String String
runJoyPretty input = pprint <$> runJoy input
