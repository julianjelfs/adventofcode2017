module Day10 where

import           Data.Bits       (xor)
import           Data.Char       (ord)
import           Data.List.Split
import qualified Data.Vector     as V
import           Numeric         (showHex)

lengths = "94,84,0,79,2,27,81,1,123,93,218,23,103,255,254,243"

suffix = [17, 31, 73, 47, 23]

parse str = fmap (\n -> read n :: Int) $ splitOn "," str

parseAscii str = foldl (\xs n -> xs ++ l) [] [0 .. 63]
  where
    l = fmap ord str ++ suffix

input = V.fromList [0 .. 255]

partOne = foldl hash (input, 0, 0) $ parse lengths

padleft str
  | length str == 1 = "0" ++ str
  | otherwise = str

partTwo =
  let (v, _, _) = foldl hash (input, 0, 0) $ parseAscii lengths
      chunked = chunksOf 16 (V.toList v)
      hex =
        fmap ((\n -> padleft $ showHex n "") . foldl xor (0 :: Integer)) chunked
  in foldl (++) mempty hex

hash (v, pos, skip) l =
  let updated = replaceSlice v pos l
      nextPos = wrapIndex 256 $ pos + l + skip
  in (updated, nextPos, skip + 1)

wrapIndex l n = mod n l

replaceSlice v from n =
  let indices = fmap (wrapIndex $ length v) [from .. (from + n - 1)]
      slice = fmap (v V.!) indices
      reversed = zip indices (reverse slice)
  in foldl (\v p -> v V.// [p]) v reversed
