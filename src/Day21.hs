module Day21 where

import qualified Common as C
import Data.Matrix hiding (transpose)
import qualified Text.Parsec as P
import Data.List.Split (chunksOf)
import Data.List (foldl', nub, transpose)
import qualified Data.Map as M

initial = [[0, 1, 0], [0, 0, 1], [1, 1, 1]]

parse = do
  inp <- readFile "data/day21.txt"
  let m = M.empty
  return $ C.parse (fmap (collapse m) (P.sepBy (ruleParser m) (P.char '\n'))) inp

ruleParser m = (,) <$> ((permutations <$> matrixParser) <* (P.string " => ")) <*> matrixParser

collapse m rules =
    foldl' foldPatterns m rules

foldPatterns m (patterns, result) =
    foldl' (\m p -> M.insert p result m) m patterns

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
    nub $ concat $ fmap (\r -> [r, flipHorizontally r, flipVertically r]) $ rotations m

splitMatrix rules m
    | ncols m < 4 = transform rules m
    | mod (ncols m) 2 == 0 = splitIntoChunks rules m 2
    | otherwise = splitIntoChunks rules m 3

splitIntoChunks rules m n =
    let s = split m
        t = (fmap . fmap) (transform rules) s
    in fmap join t
--    let s = ncols m
--        c = (div s n)
--        iter = [0..(c-1)]
--        processed =
--            foldl' (\row r ->
--                let col =
--                        foldl' (\col c ->
--                            let sr = (r * n) + 1
--                                sc = (c * n) + 1
--                                sub = submatrix sr (sr + (n-1)) sc (sc + (n-1)) m
--                                transformed = transform rules sub
--                            in
--                                case col of
--                                   Nothing -> Just transformed
--                                   Just col -> Just (col <|> transformed)
--                        ) Nothing iter
--                in case row of
--                    Nothing -> col
--                    Just row ->
--                        case col of
--                            Nothing -> Just row
--                            Just col -> Just (row <-> col)
--            ) Nothing iter
--    in
--        case processed of
--            Nothing -> m
--            Just p -> p
    where split = transpose . map (map transpose . chunksOf n . transpose) . chunksOf n
          join = transpose . concatMap (transpose . concat)

solve n = do
    rules <- parse
    return $ fmap (\r -> sum $ head $ drop n $ iterate (splitMatrix r) initial) rules

transform rules m =
    case M.lookup m rules of
        Nothing -> m
        Just r -> r
