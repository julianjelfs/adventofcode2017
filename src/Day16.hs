module Day16 where

import qualified Common as C
import Data.List
import qualified Data.Vector as V
import qualified Text.Parsec as P
import Control.Applicative
import qualified Data.Map.Strict as M

data Move
  = Spin Int
  | Exchange Int
             Int
  | Partner Char
            Char
  deriving (Show)

parse = do
  inp <- readFile "data/day16.txt"
  return $ C.parse movesParser inp

movesParser = P.sepBy moveParser (P.char ',')

moveParser = P.choice [spinParser, exchangeParser, partnerParser]

spinParser = Spin <$> (P.char 's' *> C.numberParser)

exchangeParser =
  Exchange <$> (P.char 'x' *> C.numberParser) <*> (P.char '/' *> C.numberParser)

partnerParser =
  Partner <$> (P.char 'p' *> P.anyChar) <*> (P.char '/' *> P.anyChar)

progs = V.fromList ['a'..'p']

partOne = do
  moves <- parse
  return $ fmap (\m -> doDance m progs M.empty) moves

partTwo = do
  moves <- parse
  let cache = M.empty
  return
    $ fmap V.toList
    $ fmap fst
    $ fmap (\m -> foldl' (\(p, c) _ -> doDance m p c) (progs, cache) [1..1e9]) moves

doDance moves progs cache =
  case M.lookup progs cache of
    Just p -> (p, cache)
    Nothing ->
      let p = foldl' applyMove progs moves
      in (p, M.insert progs p cache)

applyMove progs (Spin n) =
  let l = length progs
      h = V.take (l - n) progs
      t = V.drop (l - n) progs
  in t V.++ h
applyMove progs (Exchange x y) =
  let ix = progs V.! x
      iy = progs V.! y
  in progs V.// [(x, iy), (y, ix)]
applyMove progs (Partner x y) =
  case liftA2 Exchange (V.elemIndex x progs) (V.elemIndex y progs) of
    Just x -> applyMove progs x
    Nothing -> progs
