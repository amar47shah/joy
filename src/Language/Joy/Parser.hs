module Language.Joy.Parser
       (
       ) where

import           Control.Applicative                ((<$>))
import qualified Data.Map                           as M
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Char (letter)

data Joy = JoyNumber Integer
         | JoyLiteral String
         | JoyString String
         | JoyQuote [Joy]
         | JoyBool Bool
           deriving ( Show, Eq )

parseNumber :: Parser Joy
parseNumber = (JoyNumber . read) <$> many1 digit

parseString :: Parser Joy
parseString = do
    _ <- char '"'
    x <- many (noneOf "\"")
    _ <- char '"'
    return $ JoyString x

parseList :: Parser Joy
parseList =  do
    _ <- char '['
    x <- try (parseExpr `sepBy` spaces)
    _ <- char ']'
    return $ JoyQuote x

parseLiteral :: Parser Joy
parseLiteral = do
    x <- many1 letter
    return $ JoyLiteral x

parseExpr :: Parser Joy
parseExpr = parseList <|> parseNumber <|> parseString <|> parseLiteral

parseJoy :: String -> Either ParseError [Joy]
parseJoy input = parse parser "JOY" input
    where parser = many1 (spaces *> parseExpr <* spaces)

-------------------------------

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

cons (JoyQuote qs : x : xs) = JoyQuote (x : qs) : xs
cons _ = exception

-- Swaps the first two elements on the stack.
swap :: JoyF
swap (x:y:xs) = y:x:xs
swap _ = exception

-- Combinators

i :: [Joy] -> [Joy]
i ((JoyQuote x) : xs) = eval x xs

prelude :: M.Map [Char] JoyF
prelude =
    M.fromList [ ("+", add)
               , ("dup", dup)
               , ("swap", swap)
               , ("cons", cons)
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
