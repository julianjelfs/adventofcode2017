module Day25 where

import qualified Data.Map.Strict as M

data State
  = A
  | B
  | C
  | D
  | E
  | F
  deriving Show

iterations = 12386363
--iterations = 10

partOne =
  let (_, _, tape) = head $ reverse $ take iterations $ iterate step (A, 0, M.empty)
  in sum (M.elems tape)

step (s, pos, tape) =
  let v = getVal pos tape
  in case s of
      A ->
        case v of
          0 -> (B, pos+1, M.insert pos 1 tape)
          1 -> (B, pos-1, M.insert pos 0 tape)
      B ->
        case v of
          0 -> (C, pos-1, M.insert pos 1 tape)
          1 -> (A, pos+1, M.insert pos 0 tape)
      C ->
        case v of
          0 -> (D, pos-1, M.insert pos 1 tape)
          1 -> (C, pos+1, M.insert pos 0 tape)
      D ->
        case v of
          0 -> (E, pos-1, M.insert pos 1 tape)
          1 -> (F, pos-1, M.insert pos 0 tape)
      E ->
        case v of
          0 -> (A, pos-1, M.insert pos 1 tape)
          1 -> (C, pos-1, M.insert pos 1 tape)
      F ->
        case v of
          0 -> (E, pos-1, M.insert pos 1 tape)
          1 -> (A, pos+1, M.insert pos 1 tape)

getVal pos tape =
  case M.lookup pos tape of
    Nothing -> 0
    Just n -> n
