module Day10 where

import           Data.List.Split
import qualified Data.Vector     as V

lengths = "94,84,0,79,2,27,81,1,123,93,218,23,103,255,254,243"

testLengths = "3,4,1,5"

parse str = fmap (\n -> read n :: Int) $ splitOn "," str

input = V.fromList [0 .. 255]

partOne = foldl hash (input, 0, 0) $ parse lengths
  where
    inpLength = length input
    hash (v, pos, skip) l =
      let updated = replaceSlice v pos l
          nextPos = wrapIndex inpLength $ pos + l + skip
      in (updated, nextPos, skip + 1)

wrapIndex l n = mod n l

replaceSlice v from n =
  let indices = fmap (wrapIndex $ length v) [from .. (from + n - 1)]
      slice = fmap (v V.!) indices
      reversed = zip indices (reverse slice)
  in foldl (\v p -> v V.// [p]) v reversed
