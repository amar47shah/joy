-- |
-- Module      : Language.Joy.Parser
-- Copyright   : (c) 2016 Owain Lewis
--
-- License     : BSD-style
-- Maintainer  : owain@owainlewis.com
-- Stability   : experimental
-- Portability : GHC
--
-- A Joy language parser
--
module Language.Joy.Parser
       ( Joy(..)
       , parseJoy
       , parseNumber
       , parseString
       , parseComment
       , parseSymbol
       , parseAssignment
       ) where

import           Control.Applicative                ((<$>))
import           Data.Char                          (isSpace)
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Char (letter)
import           Language.Joy.AST

-----------------------------------------
-- | Parsers
-----------------------------------------

whiteSpace :: Parser a -> Parser a
whiteSpace p = many (char ' ') *> p <* many (char ' ')

lexi :: Parser a -> Parser a
lexi p = spaces *> p <* spaces

parseNumber :: Parser Joy
parseNumber = (JoyNumber . read) <$> many1 digit

-- Additions to the spec ??
parseTrue :: Parser Joy
parseTrue = (\_ -> return $ JoyBool True) =<< string "true"

parseFalse :: Parser Joy
parseFalse = (\_ -> return $ JoyBool False) =<< string "false"

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

parseSymbol :: Parser Joy
parseSymbol = do
    x <- many1 (alphaNum <|> oneOf ['+', '-', '*', '.', '\\', '>', '<', '='])
    return $ JoySymbol x

-- | Assigment in Joy
-- |
-- | @@
-- | let fac ==
-- |   [null] [succ] [dup pred] [*] linrec
-- | ;
-- | @@
-- |
parseAssignment :: Parser Joy
parseAssignment = do
    string "DEFINE"
    var <- whiteSpace $ many1 alphaNum
    lexi $ string "=="
    expr <- many1 (whiteSpace parseExpr)
    spaces *> char ';'
    return $ JoyAssignment var expr

parseExpr :: Parser Joy
parseExpr = (try parseAssignment)
        <|> parseList
        <|> parseNumber
        <|> parseString
        <|> parseTrue
        <|> parseFalse
        <|> parseSymbol
        <|> parseComment

testParser :: (Parser a) -> String -> Either ParseError a
testParser p input = parse p " " input

parseJoy :: String -> Either ParseError JoyProgram
parseJoy input = parse parser "JOY" input
    where parser = many1 (lexi parseExpr)
