module Common where

import Text.Parsec as P
import Text.ParserCombinators.Parsec(Parser, ParseError)

numberParser :: P.Parsec String st Int
numberParser = read <$> (P.many P.digit)

parse :: Parser a -> String -> Either ParseError a
parse parser = P.parse parser []
