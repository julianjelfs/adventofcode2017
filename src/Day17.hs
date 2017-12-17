module Day17 where

import Data.Sequence as S ((><), (|>), fromList, drop, take)
import Data.List as L

step = 343

buffer = fromList [0]

partOne =
  let (p, b) = foldl' nextStep (0, buffer) [1..2017]
  in S.take 1 $ S.drop (p + 1) b

partTwo =
  let (p, s, t) = foldl' nextStep' (0, 1, 0) [1..50000000]
  in (p, s, t)

nextStep' (pos, size, target) n =
  let i = nextIndex size pos step
  in (i + 1, n + 1, if i == 0 then n else target)

nextStep (pos, buffer) n =
  let i = nextIndex (length buffer) pos step
  in (i + 1, insertAfter i n buffer)

nextIndex size curr step = mod (curr + step) size

insertAfter i v buffer =
  let h = S.take (i + 1) buffer
      t = S.drop (i + 1) buffer
  in (h |> v) >< t
