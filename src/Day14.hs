module Day14 where

import Text.Printf
import Numeric
import Day10
import qualified Data.Set as S
import Data.List

inp = "hwlqcszp-"
hashes = ((inp ++) . show) <$> [0..127]

partOne =
  length $ filter (\c -> c == '1') $ (concatMap . concatMap) hexToBinary $ Day10.doHash <$>hashes

partTwo = do
    grid <- readFile "data/day14.txt"
    let coords = fst $
                    foldl'
                        (\(s, y) l ->
                            (fst $ foldl' (\(s, x) c -> (if c == '1' then S.insert (x, y) s else s, x + 1)) (s, 0) l, y + 1)
                        ) (S.empty, 0) $ lines grid
    return coords

hexToBinary c =
  case (readHex::String -> [(Int, String)]) [c] of
    ((x, _):[]) -> printf "%04b" x
    _ -> ""
