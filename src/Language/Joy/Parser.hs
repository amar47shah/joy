module Language.Joy.Parser
       ( Joy(..)
       , parseJoy
       , parseNumber
       , parseString
       , parseAssignment
       , testParser
       ) where

import           Control.Applicative                ((<$>))
import           Data.Char                          (isSpace)
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Char (letter)

type JoyProgram = [Joy]

data Joy = JoyNumber Integer
         | JoyLiteral String
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
    showJoy (JoyLiteral x) = x
    showJoy (JoyString x) = x
    showJoy (JoyQuote xs) =
        let innerForms = map showJoy xs
            lBrace = ["["]
            rBrace = ["]"]
       in
        mconcat . concat $ [lBrace, innerForms, rBrace]
    showJoy (JoyBool x) = show x
    showJoy (JoyAssignment k p) = k
    showJoy (JoyComment x) = ""

-----------------------------------------
-- | Parsers
-----------------------------------------

whiteSpace :: Parser a -> Parser a
whiteSpace p = spaces *> p <* spaces

parseNumber :: Parser Joy
parseNumber = (JoyNumber . read) <$> many1 digit

parseString :: Parser Joy
parseString = do
    _ <- char '"'
    x <- many (noneOf "\"")
    _ <- char '"'
    return $ JoyString x

parseComment :: Parser Joy
parseComment =
    let trim = reverse . dropWhile Data.Char.isSpace in
    do
        _ <- string "(*"
        body <- many (noneOf "*)")
        _ <- string "*)"
        let comment = JoyComment (trim . trim $ body)
        return comment

parseList :: Parser Joy
parseList =  do
    _ <- char '['
    x <- try (parseExpr `sepBy` spaces)
    _ <- char ']'
    return $ JoyQuote x

parseLiteral :: Parser Joy
parseLiteral = do
    x <- many1 (alphaNum <|> oneOf ['+', '-', '*'])
    return $ JoyLiteral x

-- Assigment in Joy
-- fac  == [null] [succ] [dup pred] [*] linrec
parseAssignment :: Parser Joy
parseAssignment = do
    var <- whiteSpace $ many1 alphaNum
    string "=="
    expr <- many1 (whiteSpace parseExpr)
    return $ JoyAssignment var expr

-----------------------------------------

parseExpr :: Parser Joy
parseExpr = (try parseAssignment)
        <|> parseList
        <|> parseNumber
        <|> parseString
        <|> parseLiteral
        <|> parseComment

testParser :: (Parser a) -> String -> Either ParseError a
testParser p input = parse p " " input

parseJoy :: String -> Either ParseError JoyProgram
parseJoy input = parse parser "JOY" input
    where parser = many1 (whiteSpace parseExpr)
