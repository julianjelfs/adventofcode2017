module Day8 where

import qualified Data.Map.Strict as M
import           Data.Ord

type Source = String

type Target = String

type Op = (Int -> Int)

type Pred = (Int -> Bool)

data Inst =
  Inst Source
       Target
       Op
       Pred

instance Show Inst where
  show (Inst s t o p) = "Source: " ++ s ++ " Target: " ++ t

parse = do
  inp <- readFile "data/day8.txt"
  return $ fmap (parseInst . words) $ lines inp

parseInst [] = error "empty instruction"
parseInst (register:op1:val:"if":target:op2:targetVal:[]) =
  Inst register target (parseOp op1 (read val)) (parsePred op2 (read targetVal))

parseOp "inc" v = \n -> n + v
parseOp "dec" v = \n -> n - v

parsePred ">" v  = \n -> n > v
parsePred "<" v  = \n -> n < v
parsePred ">=" v = \n -> n >= v
parsePred "==" v = \n -> n == v
parsePred "<=" v = \n -> n <= v
parsePred "!=" v = \n -> n /= v

withDefault m k d =
  case M.member k m of
    True  -> m M.! k
    False -> d

foldInstruction (m, highest) (Inst s t o p) =
  let targetVal = withDefault m t 0
      sourceVal = withDefault m s 0
  in if p targetVal
       then let updatedSource = o sourceVal
                newHighest =
                  if updatedSource > highest
                    then updatedSource
                    else highest
            in (M.insert s updatedSource m, newHighest)
       else (m, highest)

partOne = do
  instructions <- parse
  let m = foldl foldInstruction (M.empty, 0) instructions
  return $ maximum $ M.elems $ fst m

partTwo = do
  instructions <- parse
  let m = foldl foldInstruction (M.empty, 0) instructions
  return $ snd m
