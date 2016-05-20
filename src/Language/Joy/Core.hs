module Language.Joy.Core where

import           Control.Applicative ((<$>))
import qualified Data.Map            as M

import           Language.Joy.Parser (Joy (..), parseJoy)
-----------------------------------------
-- | Standard lib
-----------------------------------------

type JoyF = [Joy] -> Either String [Joy]

binOp :: (Integer -> Integer -> Integer) -> JoyF
binOp op (JoyNumber y: JoyNumber x:xs) = pure $ JoyNumber (x `op` y) : xs
binOp _ _ = Left "invalid state"

add :: JoyF
add stack = binOp (+) stack

mult :: JoyF
mult stack = binOp (*) stack

-- Swap, dup and zap

-- | Swaps the first two elements on the stack.
-- [B] [A] swap == [A] [B]
swap :: JoyF
swap (x:y:xs) = pure $ y:x:xs
swap _ = Left "invalid state"

-- [A] dup  == [A] [A]
dup :: JoyF
dup (x:xs) = pure (x:x:xs)
dup _ = Left "invalid state"

-- [A] zap  ==
zap :: JoyF
zap (x:xs) = pure xs
zap _ = Left "invalid state"

-- Cat, cons and unit
cons :: JoyF
cons (JoyQuote qs : x : xs) = pure $ JoyQuote (x : qs) : xs
cons _ = Left "invalid state"

-- Cat takes two quotations and concatenates them together
cat :: JoyF
cat (JoyQuote xs : JoyQuote ys : tl) = pure $ JoyQuote (xs ++ ys) : tl
cat _ = Left "invalid state"

first :: JoyF
first (JoyQuote (x:xs) : ys) = pure $ x : JoyQuote xs : ys
first _ = Left "invalid state"

-- [map +]

-----------------------------------------
-- | Combinators
-----------------------------------------

-----------------------------------------
-- | Prelude env
-----------------------------------------

prelude :: M.Map String JoyF
prelude =
    M.fromList [ ("+", add)
               , ("*", mult)
               , ("swap", swap)
               , ("dup", dup)
               , ("zap", zap)
               , ("cons", cons)
               , ("cat", cat)
               , ("first", first)
               ]
-----------------------------------------
-- | Compiler
-----------------------------------------

type RuntimeStack = [Joy]
type ProgramStack = [Joy]

-- square = dup *

eval :: RuntimeStack -> ProgramStack -> M.Map String [Joy] -> [Joy]
eval s [] env = s
eval stack (value@(JoyBool _) : xs) env =
    eval (value : stack) xs env
eval stack (value@(JoyNumber _) : xs) env =
    eval (value : stack) xs env
eval stack (value@(JoyQuote _) : xs) env =
    eval (value : stack) xs env
eval stack (value@(JoyAssignment k f) : xs) env =
    eval stack xs (M.insert k f env)
eval stack (value@(JoyLiteral l) : xs) env =
    case (M.lookup l prelude) of
      -- Try the native env first
      Just f -> case (f stack) of
                  Left e ->
                    -- Try the user defined env
                    case (M.lookup l env) of
                      Just p -> eval stack xs env
                      Nothing -> error e
                  Right s -> eval s xs env
      Nothing -> error $ "Unbound literal " ++ l

runJoy :: String -> Either String [Joy]
runJoy input =
    case (parseJoy input) of
      Right program -> Right $ eval [] program M.empty
      Left e -> Left (show e)
