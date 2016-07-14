-- |
-- Module      : Language.Joy.Parser
-- Copyright   : (c) 2016 Owain Lewis
--
-- License     : BSD-style
-- Maintainer  : owain@owainlewis.com
-- Stability   : experimental
-- Portability : GHC
--
-- The primary AST for Joy
--
module Language.Joy.AST
    ( Joy(..)
    , JoyProgram
    , JoyError(..)
    , pprint
    ) where

import qualified Data.List as L

type JoyProgram = [Joy]

data JoyError = InvalidState String
              | RuntimeError String
    deriving ( Eq, Show )

data Joy = JoyNumber Integer
         | JoySymbol String
         | JoyString String
         | JoyQuote [Joy]
         | JoyBool Bool
         | JoyAssignment String JoyProgram
         | JoyComment String
           deriving ( Eq, Show )

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
