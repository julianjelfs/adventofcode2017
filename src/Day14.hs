module Day14 where

import Text.Printf
import Numeric
import Day10
import qualified Data.Set as S
import Data.List
import Data.Maybe

inp = "hwlqcszp-"
hashes = ((inp ++) . show) <$> [0..127]

type Coords = S.Set (Int, Int)

partOne =
  length $ filter (\c -> c == '1') $ (concatMap . concatMap) hexToBinary $ Day10.doHash <$>hashes

partTwo = do
    grid <- readFile "data/day14.txt"
    let coords = fst $
                    foldl'
                        (\(s, y) l ->
                            (fst $ foldl' (\(s, x) c -> (if c == '1' then S.insert (x, y) s else s, x + 1)) (s, 0) l, y + 1)
                        ) (S.empty, 0) $ lines grid
    return $ coords

--answer is too high 1302
countGroups :: Int -> Coords -> Int
countGroups n coords
    | length coords == 0 = n
    | otherwise =
        let h = (head . S.toList) coords
        in countGroups (n + 1) $ S.difference coords $ S.fromList $ neighbours coords h

neighbours :: Coords -> (Int, Int) -> [(Int, Int)]
neighbours coords (x, y) =
    (x, y) : (concatMap (neighbours updatedCoords) $ around)
    where around = filter (\c -> S.member c coords) [(x+1, y), (x, y-1), (x, y+1)]
          updatedCoords = S.difference coords $ S.fromList around

hexToBinary c =
  case (readHex::String -> [(Int, String)]) [c] of
    ((x, _):[]) -> printf "%04b" x
    _ -> ""
