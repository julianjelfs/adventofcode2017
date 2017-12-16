module Day16 where

import Common
import Data.List.Split
import qualified Data.Vector as V
import Text.Parsec as P
import Text.ParserCombinators.Parsec (Parser)

data Move
  = Spin Int
  | Exchange Int
             Int
  | Partner Char
            Char
  deriving (Show)

parse = do
  inp <- readFile "data/day16.txt"
  return $ Common.parse movesParser inp

movesParser = P.sepBy moveParser (P.char ',')

moveParser = P.choice [spinParser, exchangeParser, partnerParser]

spinParser = Spin <$> (P.char 's' *> Common.numberParser)

exchangeParser =
  Exchange <$> (P.char 'x' *> Common.numberParser) <*>
  (P.char '/' *> Common.numberParser)

partnerParser =
  Partner <$> (P.char 'p' *> P.anyChar) <*> (P.char '/' *> P.anyChar)
