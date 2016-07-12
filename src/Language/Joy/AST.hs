module Language.Joy.AST where

import qualified Data.List                          as L

type JoyProgram = [Joy]

data Joy = JoyNumber Integer
         | JoySymbol String
         | JoyString String
         | JoyQuote [Joy]
         | JoyBool Bool
         | JoyAssignment String JoyProgram
         | JoyComment String
           deriving ( Show, Eq )

class Pretty a where
    showJoy :: a -> String

instance Pretty Joy where
    showJoy (JoyNumber x) = show x
    showJoy (JoySymbol x) = x
    showJoy (JoyString x) = x
    showJoy (JoyQuote xs) =
        let innerForms = map showJoy xs
            lBrace = [" ["]
            rBrace = ["] "]
        in
        mconcat . concat $ [lBrace, innerForms, rBrace]
    showJoy (JoyBool x) = show x
    showJoy (JoyAssignment k p) = k
    showJoy (JoyComment x) = ""

-- Pretty print the stack
pprint :: Pretty a => [a] -> String
pprint xs = mconcat ["[", parts, "]"]
    where parts = L.intercalate " " (map showJoy xs)
