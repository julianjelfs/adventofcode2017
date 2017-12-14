module Day10 where

import           Data.Bits       (xor)
import           Data.Char       (ord)
import           Data.List
import           Data.List.Split
import           Data.Vector     (fromList, toList, (!), (//))
import           Text.Printf     (printf)

lengths = "94,84,0,79,2,27,81,1,123,93,218,23,103,255,254,243"

input = fromList [0 .. 255]

parse str = fmap (\n -> read n :: Int) $ splitOn "," str

parseAscii str =
  concat $ take 64 $ repeat $ fmap ord str ++ [17, 31, 73, 47, 23]

partOne = foldl hash (input, 0, 0) $ parse lengths

partTwo =
  doHash lengths

doHash str =
  let (v, _, _) = foldl hash (input, 0, 0) $ parseAscii str
      chunked = chunksOf 16 (toList v)
  in foldl (++) mempty $ fmap hexify chunked
  where
    hexify = ((printf "%02x" :: Integer -> String) . foldl xor (0 :: Integer))

hash (v, pos, skip) l =
  (replaceSlice v pos l, wrapIndex $ pos + l + skip, skip + 1)

wrapIndex n = mod n $ length input

replaceSlice v from n =
  let indices = fmap wrapIndex [from .. (from + n - 1)]
      slice = fmap (v !) indices
      reversed = zip indices (reverse slice)
  in foldl (\v p -> v // [p]) v reversed
