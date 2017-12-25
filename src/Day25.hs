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

solve =
  step 0 (A, 0, M.empty)

step 12386363 (s, pos, tape) = sum (M.elems tape)
step iterations (s, pos, tape) =
  let v = getVal pos tape
      state =
        case s of
          A ->
            case v of
              0 -> (B, pos+1, M.insert pos 1 tape)
              1 -> (E, pos-1, M.insert pos 0 tape)
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
  in step (iterations + 1) state

getVal pos tape =
  case M.lookup pos tape of
    Nothing -> 0
    Just n -> n
