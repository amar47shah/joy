module Language.Joy.Core where

import           Control.Applicative ((<$>))
import qualified Data.Map            as M

import           Language.Joy.Parser

-----------------------------------------
-- | Standard lib
-----------------------------------------

type JoyF = [Joy] -> Either String [Joy]

add :: JoyF
add (JoyNumber x: JoyNumber y:xs) = pure $ JoyNumber (x + y) : xs
add _ = Left "invalid state"

dup :: JoyF
dup (x:xs) = pure (x:x:xs)
dup _ = Left "invalid state"

cons (JoyQuote qs : x : xs) = pure $ JoyQuote (x : qs) : xs
cons _ = Left "invalid state"

-- | Swaps the first two elements on the stack.
swap :: JoyF
swap (x:y:xs) = pure $ y:x:xs
swap _ = Left "invalid state"

-----------------------------------------
-- | Combinators
-----------------------------------------

i :: [Joy] -> [Joy]
i ((JoyQuote x) : xs) = eval x xs

-----------------------------------------
-- | Prelude env
-----------------------------------------

prelude :: M.Map [Char] JoyF
prelude =
    M.fromList [ ("+", add)
               , ("dup", dup)
               , ("swap", swap)
               , ("cons", cons)
               ]

-----------------------------------------
-- | Compiler
-----------------------------------------

type RuntimeStack = [Joy]
type ProgramStack = [Joy]

eval :: RuntimeStack -> ProgramStack -> [Joy]
eval s [] = s
eval stack (value@(JoyBool _) : xs) = eval (value : stack) xs
eval stack (value@(JoyNumber _) : xs) = eval (value : stack) xs
eval stack (value@(JoyQuote _) : xs) = eval (value : stack) xs
eval stack (value@(JoyLiteral l) : xs) = case (M.lookup l prelude) of
                                             Just f ->
                                               case (f stack) of
                                                 Left e -> error e
                                                 Right s -> eval s xs
                                             Nothing -> error "Unbound literal"
