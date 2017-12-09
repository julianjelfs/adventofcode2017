module Day8 where

import qualified Data.Map.Strict as M

type Source = String

type Target = String

type Op = (Int -> Int)

type Pred = (Int -> Bool)

data Inst =
  Inst Source
       Target
       Op
       Pred

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

foldInstruction (m, highest) (Inst s t o p) =
  let targetVal = maybe 0 id (M.lookup t m)
  in if p targetVal
       then let updatedSource = o (maybe 0 id (M.lookup s m))
            in (M.insert s updatedSource m, (max updatedSource highest))
       else (m, highest)

solve = do
  instructions <- parse
  let m = foldl foldInstruction (M.empty, 0) instructions
  putStrLn $ show $ maximum $ M.elems $ fst m
  putStrLn $ show $ snd m
