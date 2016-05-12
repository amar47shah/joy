module Language.Joy.Parser
       ( Joy(..)
       , parseJoy
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

-----------------------------------------
-- | Parsers
-----------------------------------------
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

-----------------------------------------

parseExpr :: Parser Joy
parseExpr = parseList <|> parseNumber <|> parseString <|> parseLiteral <|> parseComment

parseJoy :: String -> Either ParseError JoyProgram
parseJoy input = parse parser "JOY" input
    where parser = many1 (spaces *> parseExpr <* spaces)
