module Day21 where

import qualified Common as C
import Data.Matrix
import qualified Text.Parsec as P

initial = fromList 3 3 [0, 1, 0, 0, 0, 1, 1, 1, 1]

--parse
parse = do
  inp <- readFile "data/day21.txt"
  return $ C.parse (P.sepBy ruleParser (P.char '\n')) inp

ruleParser = (,) <$> (matrixParser <* (P.string " => ")) <*> matrixParser

matrixParser = (matrixFromList . concat) <$> P.sepBy rowParser (P.char '/')

matrixFromList list
  | length list == 4 = fromList 2 2 list
  | length list == 9 = fromList 3 3 list
  | length list == 16 = fromList 4 4 list
  | otherwise = error "didn't expect that"

rowParser = P.many1 $ P.choice [const 1 <$> P.char '#', const 0 <$> P.char '.']

--findMatchingRule m rules =
--  filter (matches m) rules
--  where matches m r = any (\r -> r == m) $ permute r
--        permute r = undefined
flipVertically = fromLists . reverse . toLists

flipHorizontally = fromLists . (fmap reverse) . toLists

rotate = fromLists . reverse . toLists . transpose

rotations = (take 4) . (iterate rotate)
--3 x 3 matrix and 2 x 2 matrix
--rotate and flip matrices
--rotate is like transpose and reverse
--split matrix
