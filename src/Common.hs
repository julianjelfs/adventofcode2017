module Common where

import Text.Parsec
import Text.ParserCombinators.Parsec(Parser, ParseError)
import Control.Applicative hiding ((<|>))

numberParser :: Parsec String st Int
numberParser = rd <$> (plus <|> minus <|> number)
    where rd     = read :: String -> Int
          plus   = char '+' *> number
          minus  = (:) <$> char '-' <*> number
          number = many1 digit

parse :: Parser a -> String -> Either ParseError a
parse parser = Text.Parsec.parse parser []

--Elm style left to right fn application
(|>) :: a -> (a -> b) -> b
(|>) a atob = atob a
