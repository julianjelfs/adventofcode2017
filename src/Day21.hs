module Day21 where

import qualified Common as C
import Data.Matrix
import qualified Text.Parsec as P
import Data.List.Split (chunksOf)
import Data.List (foldl', nub)
import qualified Data.Map as M

initial = fromList 3 3 [0, 1, 0, 0, 0, 1, 1, 1, 1]

parse = do
  inp <- readFile "data/day21.txt"
  return $ C.parse (P.sepBy ruleParser (P.char '\n')) inp

ruleParser = (,) <$> ((permutations <$> matrixParser) <* (P.string " => ")) <*> matrixParser

matrixParser = (matrixFromList . concat) <$> P.sepBy rowParser (P.char '/')

matrixFromList list
  | length list == 4 = fromList 2 2 list
  | length list == 9 = fromList 3 3 list
  | length list == 16 = fromList 4 4 list
  | otherwise = error "didn't expect that"

rowParser = P.many1 $ P.choice [const 1 <$> P.char '#', const 0 <$> P.char '.']

flipVertically = fromLists . reverse . toLists

flipHorizontally = fromLists . (fmap reverse) . toLists

rotate = fromLists . reverse . toLists . transpose

rotations = (take 4) . (iterate rotate)

permutations m =
    nub $ concat $ fmap (\r -> [r, flipHorizontally r, flipVertically r]) $ rotations m

splitMatrix rules m
    | ncols m < 4 = transform rules m
    | mod (ncols m) 2 == 0 = splitIntoChunks rules m 2
    | otherwise = splitIntoChunks rules m 3

splitIntoChunks rules m n =
    let s = ncols m
        c = (div s n)
        iter = [0..(c-1)]
        processed =
            foldl' (\row r ->
                let col =
                        foldl' (\col c ->
                            let sr = (r * n) + 1
                                sc = (c * n) + 1
                                sub = submatrix sr (sr + (n-1)) sc (sc + (n-1)) m
                                transformed = transform rules sub
                            in
                                case col of
                                   Nothing -> Just transformed
                                   Just col -> Just (col <|> transformed)
                        ) Nothing iter
                in case row of
                    Nothing -> col
                    Just row ->
                        case col of
                            Nothing -> Just row
                            Just col -> Just (row <-> col)
            ) Nothing iter
    in
        case processed of
            Nothing -> m
            Just p -> p

solve n = do
    rules <- parse
    let cache = M.empty
    return $ fmap (\r -> sum $ toList $ head $ drop n $ iterate (splitMatrix r) initial) rules

transform rules m =
    case filter patternMatch rules of
        [] -> m     --no matches no transform
        (h:_) -> snd h  --match return the result
    where patternMatch (patterns, result) = any (\p -> p == m) patterns
