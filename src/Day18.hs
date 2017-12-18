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
  | Jgz InstrType
  deriving Show

parse = do
  inp <- readFile "data/day18.txt"
  return $ fmap (C.parse instrParser) $ lines inp

targetParser =
  P.choice [(Register <$> P.anyChar), (Number <$> C.numberParser)]

instrParser =
  P.choice
    [ P.try $ P.string "snd " *> (Snd <$> targetParser)
    , P.string "set " *> (Set <$> (targetParser <* P.char ' ') <*> targetParser)
    ]

--instrParser =
--  P.choice
--    [ Snd <$> (P.string "snd ") <*> targetParser
--    , Set <$> (P.string "set ") <*> targetParser <* P.char ' ' <*> targetParser
--    , Add <$> (P.string "add ") <*> targetParser <* P.char ' ' <*> targetParser
--    , Mul <$> (P.string "mul ") <*> targetParser <* P.char ' ' <*> targetParser
--    , Mod <$> (P.string "mod ") <*> targetParser <* P.char ' ' <*> targetParser
--    , Rcv <$> (P.string "rcv ") <*> targetParser
--    , Jgz <$> (P.string "jgz ") <*> targetParser <* P.char ' ' <*> targetParser
--    ]

partOne = do
  instr <- parse
  return "boooooring"