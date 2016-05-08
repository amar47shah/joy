module Language.Joy.Parser where

data Joy = JoyNumber Integer
         | JoyLiteral String
         | JoyString String
         | JoyQuote [Joy]
         | JoyBool Bool
           deriving ( Show, Eq )

-- Lists

-- Combinators

-- Compiler

-- The evaluator for a Joy program
-- The first list is the Joy runtime stack
-- The second list is the Joy input stack
-- The third list is the output of evaluation
eval :: [Joy] -> [Joy] -> [Joy]
eval s [] = s
-- When evaluating primative values we just put them onto the runtime stack
eval stack (value@(JoyBool _) : xs) = eval (value : stack) xs
eval stack (value@(JoyNumber _) : xs) = eval (value : stack) xs
