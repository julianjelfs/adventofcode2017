module Day25 where

import qualified Data.Map.Strict as M

data State
  = A | B | C | D | E | F
  deriving Show

solve =
  step 0 (A, 0, M.empty)

step 12386363 (s, pos, tape) = sum (M.elems tape)
step iterations (s, pos, tape) =
  let v = getVal pos tape
      state =
        case (s, v) of
          (A, 0) -> (B, pos+1, M.insert pos 1 tape)
          (A, 1) -> (E, pos-1, M.insert pos 0 tape)
          (B, 0) -> (C, pos-1, M.insert pos 1 tape)
          (B, 1) -> (A, pos+1, M.insert pos 0 tape)
          (C, 0) -> (D, pos-1, M.insert pos 1 tape)
          (C, 1) -> (C, pos+1, M.insert pos 0 tape)
          (D, 0) -> (E, pos-1, M.insert pos 1 tape)
          (D, 1) -> (F, pos-1, M.insert pos 0 tape)
          (E, 0) -> (A, pos-1, M.insert pos 1 tape)
          (E, 1) -> (C, pos-1, M.insert pos 1 tape)
          (F, 0) -> (E, pos-1, M.insert pos 1 tape)
          (F, 1) -> (A, pos+1, M.insert pos 1 tape)
  in step (iterations + 1) state

getVal pos tape =
  case M.lookup pos tape of
    Nothing -> 0
    Just n -> n
