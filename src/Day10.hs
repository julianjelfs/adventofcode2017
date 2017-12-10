module Day10 where

import           Data.Bits       (xor)
import           Data.Char       (ord)
import           Data.List.Split
import           Data.Vector     (fromList, toList, (!), (//))
import           Numeric         (showHex)

lengths = "94,84,0,79,2,27,81,1,123,93,218,23,103,255,254,243"

input = fromList [0 .. 255]

parse str = fmap (\n -> read n :: Int) $ splitOn "," str

parseAscii str =
  concat $ take 64 $ repeat $ fmap ord str ++ [17, 31, 73, 47, 23]

partOne = foldl hash (input, 0, 0) $ parse lengths

padleft str
  | length str == 1 = "0" ++ str
  | otherwise = str

partTwo =
  let (v, _, _) = foldl hash (input, 0, 0) $ parseAscii lengths
      chunked = chunksOf 16 (toList v)
      hex =
        fmap ((\n -> padleft $ showHex n "") . foldl xor (0 :: Integer)) chunked
  in foldl (++) mempty hex

hash (v, pos, skip) l =
  (replaceSlice v pos l, wrapIndex 256 $ pos + l + skip, skip + 1)

wrapIndex l n = mod n l

replaceSlice v from n =
  let indices = fmap (wrapIndex $ length v) [from .. (from + n - 1)]
      slice = fmap (v !) indices
      reversed = zip indices (reverse slice)
  in foldl (\v p -> v // [p]) v reversed
