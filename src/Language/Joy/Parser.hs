module Language.Joy.Parser
       ( Joy(..)
       , parseJoy
       ) where

import           Control.Applicative                ((<$>))
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Char (letter)

data Joy = JoyNumber Integer
         | JoyLiteral String
         | JoyString String
         | JoyQuote [Joy]
         | JoyBool Bool
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
parseExpr = parseList <|> parseNumber <|> parseString <|> parseLiteral

parseJoy :: String -> Either ParseError [Joy]
parseJoy input = parse parser "JOY" input
    where parser = many1 (spaces *> parseExpr <* spaces)
