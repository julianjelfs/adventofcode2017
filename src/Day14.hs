module Day14 where

import Text.Printf
import Numeric
import Day10

inp = "hwlqcszp-"

partOne =
  let hashes = ((inp ++) . show) <$> [0..127]
  in length $ filter (\c -> c == '1') $ concatMap hashToBinary $ Day10.doHash <$>hashes

hashToBinary =
  concatMap hexToBinary

hexToBinary c =
  case (readHex::String -> [(Int, String)]) [c] of
    ((x, _):[]) -> printf "%04b" x
    _ -> ""
