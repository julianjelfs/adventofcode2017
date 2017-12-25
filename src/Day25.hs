module Day25 where

import qualified Data.Map.Strict as M

data State
  = A | B | C | D | E | F
  deriving Show

solve =
  step 0 (A, 0, M.empty)

step 12386363 (s, pos, tape) = sum (M.elems tape)
step iterations (s, pos, tape) =
  let v = getValue pos tape
      state =
        case (s, v) of
          (A, 0) -> (B, pos+1, setValue 1)
          (A, 1) -> (E, pos-1, setValue 0)
          (B, 0) -> (C, pos-1, setValue 1)
          (B, 1) -> (A, pos+1, setValue 0)
          (C, 0) -> (D, pos-1, setValue 1)
          (C, 1) -> (C, pos+1, setValue 0)
          (D, 0) -> (E, pos-1, setValue 1)
          (D, 1) -> (F, pos-1, setValue 0)
          (E, 0) -> (A, pos-1, setValue 1)
          (E, 1) -> (C, pos-1, setValue 1)
          (F, 0) -> (E, pos-1, setValue 1)
          (F, 1) -> (A, pos+1, setValue 1)
  in step (iterations + 1) state
  where setValue n = M.insert pos n tape
        getValue p t =
          case M.lookup p t of
            Nothing -> 0
            Just n -> n
