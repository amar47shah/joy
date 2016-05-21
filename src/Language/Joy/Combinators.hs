module Language.Joy.Combinators where

import           Language.Joy.Parser

type JoyF = [Joy] -> Either String [Joy]

-----------------------------------------
-- | Joy Combinators
-----------------------------------------

i :: JoyF
i ((JoyQuote qs) : xs) = pure $ qs ++ xs
i _ = Left "invalid state"
