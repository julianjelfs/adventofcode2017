module Day21 where

import qualified Common as C
import Data.List (foldl', nub, transpose)
import Data.List.Split (chunksOf)
import qualified Data.Map as M
import Data.Matrix hiding (transpose)
import qualified Text.Parsec as P

initial = [[0, 1, 0], [0, 0, 1], [1, 1, 1]]

parse = do
  inp <- readFile "data/day21.txt"
  let m = M.empty
  return $ C.parse (fmap (collapse m) (P.sepBy (ruleParser m) (P.char '\n'))) inp

ruleParser m =
  (,) <$> (matrixParser <* (P.string " => ")) <*>
  matrixParser

collapse m rules = foldl' foldPatterns m rules

foldPatterns m (pattern, result) =
  foldl' (\m p -> M.insert p result m) m (permutations pattern)

matrixParser = (matrixFromList . concat) <$> P.sepBy rowParser (P.char '/')

matrixFromList list
  | length list == 4 = toLists $ fromList 2 2 list
  | length list == 9 = toLists $ fromList 3 3 list
  | length list == 16 = toLists $ fromList 4 4 list
  | otherwise = error "didn't expect that"

rowParser = P.many1 $ P.choice [const 1 <$> P.char '#', const 0 <$> P.char '.']

flipVertically = reverse

flipHorizontally = fmap reverse

rotate = reverse . transpose

rotations = (take 4) . (iterate rotate)

permutations m =
  nub $
  concat $ fmap (\r -> [r, flipHorizontally r, flipVertically r]) $ rotations m

solve n = do
  rules <- parse
  return $
    fmap (\r -> onCount $ head $ drop n $ iterate (splitMatrix r) initial) rules

onCount g =
  foldl' (\t r -> t + (sum r)) 0 g

transform rules m =
  case M.lookup m rules of
    Nothing -> m
    Just r -> r

splitMatrix rules m
  | length m < 4 = transform rules m
  | mod (length m) 2 == 0 = splitIntoChunks rules m 2
  | otherwise = splitIntoChunks rules m 3

splitIntoChunks rules m n =
  let s = split m
      t = (fmap . fmap) (transform rules) s
  in join t
  where
    split = transpose . map (map transpose . chunksOf n . transpose) . chunksOf n
    join = transpose . concatMap (transpose . concat)
