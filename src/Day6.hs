module Day6 where

import qualified Data.Vector as V
import Data.Ord
import qualified Data.Set as S

input = V.fromList [0,5,10,0,11,14,13,4,11,8,8,7,1,4,12,11]
testInput = V.fromList [0,2,7,0]

maxi xs = V.maximumBy (comparing fst) (V.zip xs (V.fromList [0..(length xs)]))

balance v =
  let
    (m, i) = maxi v
    v1 = v V.// [(i, 0)]
  in distribute m (next i v1) v1

distribute 0 i v = v
distribute m i v =
  distribute (m - 1) (next i v) (v V.// [(i, (v V.! i) + 1)])

findCycle previous v cycles =
  let
    v1 = balance v
  in
    if S.member v1 previous then
      cycles
    else
      findCycle (S.insert v1 previous) v1 (cycles + 1)

partOne =
  findCycle S.empty input 1

next i v =
  mod (i + 1) (length v)


