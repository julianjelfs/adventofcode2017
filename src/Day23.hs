module Day23 where

import qualified Common as C
import Data.Char
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import qualified Text.Parsec as P
import Data.Numbers.Primes

data InstrType
  = Number Int
  | Register Char
  deriving (Show)

data Instr
  = Set Char InstrType
  | Sub Char InstrType
  | Mul Char InstrType
  | Jnz InstrType InstrType
  deriving (Show)

data Program = Program
  { register :: M.Map Char Int
  , nmuls :: Int
  , pos :: Int
  , finished :: Bool
  }
  deriving Show

parse = do
  inp <- readFile "data/day23.txt"
  return $ fmap V.fromList $ C.parse (P.sepBy instrParser (P.char '\n')) inp

targetParser = P.choice [(Number <$> C.numberParser), (Register <$> P.anyChar)]

instrParser =
  P.choice
    [ P.try $ P.string "set " *> (Set <$> (P.anyChar <* P.char ' ') <*> targetParser)
    , P.try $ P.string "sub " *> (Sub <$> (P.anyChar <* P.char ' ') <*> targetParser)
    , P.try $ P.string "mul " *> (Mul <$> (P.anyChar <* P.char ' ') <*> targetParser)
    , P.try $ P.string "jnz " *> (Jnz <$> (targetParser <* P.char ' ') <*> targetParser)
    ]

getValue reg (Number n) = n
getValue reg (Register a) =
  case M.lookup a reg of
    Just n -> n
    Nothing -> 0

partOne = do
  instr <- parse
  let p0 = Program M.empty 0 0 False
  return $ fmap (runProgram (p0, [])) instr

runProgram (p, progs) instr =
  let p' = stepProgram instr p
  in if (finished p')
       then p'
       else runProgram (p', (p':progs)) instr

stepProgram instr prog@(Program { register = register
                                , nmuls = nmuls
                                , pos = pos
                                , finished = finished
                                })
  | pos < 0 || pos >= (length instr) = prog {finished = True}
  | otherwise =
    case instr V.! pos of
      Set x y ->
        prog
          { register = M.insert x (getValue register y) register
          , pos = pos + 1
          }
      Sub x y -> binaryOp prog x y (-)
      Mul x y -> (binaryOp prog x y (*)) { nmuls = nmuls + 1 }
      Jnz x y ->
        if (getValue register x) /= 0
          then prog {pos = pos + (getValue register y)}
          else prog {pos = pos + 1}

binaryOp prog@(Program {pos = pos, register = register}) x y op =
  let xval = getValue register (Register x)
      yval = getValue register y
  in prog {register = M.insert x (op xval yval) register, pos = pos + 1}

--this is what the processor is doing
partTwo =
  let a = 1
      b = 67 * 100 + 100000
      c = b + 17000
      g = b - c
  in go (g, 0, b, c)
  where
    go (g, h, b, c)
      | g == 0 = h
      | otherwise =
          if isPrime b then
            go (b - c, h, b + 17, c)
          else
            go (b - c, h + 1, b + 17, c)
