module Language.Joy.Parser
       (
       ) where

import qualified Data.Map as M

data Joy = JoyNumber Integer
         | JoyLiteral String
         | JoyString String
         | JoyQuote [Joy]
         | JoyBool Bool
           deriving ( Show, Eq )

exception = error "Invalid state"

-----------------------------------------
-- Prelude
-----------------------------------------

type JoyF = [Joy] -> [Joy]

add :: JoyF
add (JoyNumber x: JoyNumber y:xs) = JoyNumber (x + y) : xs
add _ = exception

dup :: JoyF
dup (x:xs) = x:x:xs
dup _ = exception

-- Combinators

i :: [Joy] -> [Joy]
i ((JoyQuote x) : xs) = eval x xs

prelude :: M.Map [Char] JoyF
prelude =
    M.fromList [ ("+", add)
               , ("dup", dup)
               ]
-----------------------------------------

-- Compiler

type RuntimeStack = [Joy]
type ProgramStack = [Joy]

-- | The evaluator for a Joy program
-- The first list is the Joy runtime stack
-- The second list is the Joy input stack
-- The third list is the output of evaluation
eval :: RuntimeStack -> ProgramStack -> [Joy]
eval s [] = s
eval stack (value@(JoyBool _) : xs) = eval (value : stack) xs
eval stack (value@(JoyNumber _) : xs) = eval (value : stack) xs
eval stack (value@(JoyQuote _) : xs) = eval (value : stack) xs
eval stack (value@(JoyLiteral l) : xs) = case (M.lookup l prelude) of
                                             Just f -> eval (f stack) xs
                                             Nothing -> error "Unbound literal"
