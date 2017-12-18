module Day18 where

import qualified Common as C
import qualified Data.Map.Strict as M
import qualified Text.Parsec as P
import Data.Char

data InstrType
  = Number Int
  | Register Char
  deriving Show

data Instr
  = Snd InstrType
  | Set InstrType InstrType
  | Add InstrType InstrType
  | Mul InstrType InstrType
  | Mod InstrType InstrType
  | Rcv InstrType
  | Jgz InstrType InstrType
  deriving Show

parse = do
  inp <- readFile "data/day18.txt"
  return $ C.parse (P.sepBy instrParser (P.char '\n')) inp

targetParser =
  P.choice [(Number <$> C.numberParser), (Register <$> P.anyChar)]

instrParser =
  P.choice
    [ P.try $ P.string "snd " *> (Snd <$> targetParser)
    , P.try $ P.string "set " *> (Set <$> (targetParser <* P.char ' ') <*> targetParser)
    , P.try $ P.string "add " *> (Add <$> (targetParser <* P.char ' ') <*> targetParser)
    , P.try $ P.string "mul " *> (Mul <$> (targetParser <* P.char ' ') <*> targetParser)
    , P.try $ P.string "mod " *> (Mod <$> (targetParser <* P.char ' ') <*> targetParser)
    , P.try $ P.string "rcv " *> (Rcv <$> targetParser)
    , P.try $ P.string "jgz " *> (Jgz <$> (targetParser <* P.char ' ') <*> targetParser)
    ]

partOne = do
  instr <- parse
  return "boooooring"
