module Day18 where

import qualified Common as C
import Data.Char
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import qualified Text.Parsec as P

data InstrType
  = Number Int
  | Register Char
  deriving (Show)

data Instr
  = Snd InstrType
  | Set Char
        InstrType
  | Add Char
        InstrType
  | Mul Char
        InstrType
  | Mod Char
        InstrType
  | Rcv Char
  | Jgz InstrType
        InstrType
  deriving (Show)

data Program = Program
  { progId :: Int
  , register :: M.Map Char Int
  , pos :: Int
  , sent :: Int
  , waiting :: Bool
  , finished :: Bool
  , queue :: [Int]
  , lastSent :: Int
  }

parse = do
  inp <- readFile "data/day18.txt"
  return $ fmap V.fromList $ C.parse (P.sepBy instrParser (P.char '\n')) inp

targetParser = P.choice [(Number <$> C.numberParser), (Register <$> P.anyChar)]

instrParser =
  P.choice
    [ P.try $ P.string "snd " *> (Snd <$> targetParser)
    , P.try $
      P.string "set " *> (Set <$> (P.anyChar <* P.char ' ') <*> targetParser)
    , P.try $
      P.string "add " *> (Add <$> (P.anyChar <* P.char ' ') <*> targetParser)
    , P.try $
      P.string "mul " *> (Mul <$> (P.anyChar <* P.char ' ') <*> targetParser)
    , P.try $
      P.string "mod " *> (Mod <$> (P.anyChar <* P.char ' ') <*> targetParser)
    , P.try $ P.string "rcv " *> (Rcv <$> P.anyChar)
    , P.try $
      P.string "jgz " *> (Jgz <$> (targetParser <* P.char ' ') <*> targetParser)
    ]

getValue progId reg (Number n) = n
getValue progId reg (Register a) =
  case M.lookup a reg of
    Just n -> n
    Nothing ->
      if a == 'p'
        then progId
        else 0

partOne = do
  instr <- parse
  let p0 = Program 0 M.empty 0 0 False False [] 0
  return $ fmap (runProgram p0) instr

runProgram p0 instr =
  let (p0', _) = stepProgram instr p0
  in if (finished p0' || waiting p0')
       then lastSent p0'
       else runProgram p0' instr

partTwo = do
  instr <- parse
  let p0 = Program 0 M.empty 0 0 False False [] 0
      p1 = Program 1 M.empty 0 0 False False [] 0
  return $ fmap (runPrograms p0 p1) instr

runPrograms p0 p1 instr =
  let (p0', m1) = stepProgram instr p0
      (p1', m0) = stepProgram instr p1
  in if (finished p0' && finished p1') || deadlocked p0' p1'
       then sent p1'
       else runPrograms (addToQueue p0' m0) (addToQueue p1' m1) instr

addToQueue prog mmsg =
  case mmsg of
    Nothing -> prog
    Just msg -> prog {queue = (queue prog) ++ [msg]}

deadlocked p0 p1 = waiting p0 && waiting p1

stepProgram instr prog@(Program { progId = progId
                                , pos = pos
                                , register = register
                                , sent = sent
                                , queue = queue
                                })
  | pos < 0 || pos >= (length instr) = do (prog {finished = True}, Nothing)
  | otherwise =
    case instr V.! pos of
      Rcv n ->
        case queue of
          [] -> (prog {waiting = True}, Nothing)
          (x:t) ->
            ( prog
              { register = M.insert n x register
              , queue = t
              , pos = pos + 1
              , waiting = False
              }
            , Nothing)
      Snd n ->
        let send = (getValue progId register n)
        in (prog {pos = pos + 1, sent = sent + 1, lastSent = send}, Just send)
      Set a b ->
        ( prog
          { register = M.insert a (getValue progId register b) register
          , pos = pos + 1
          }
        , Nothing)
      Add a b -> binaryOp prog a b (+)
      Mul a b -> binaryOp prog a b (*)
      Mod a b -> binaryOp prog a b mod
      Jgz a b ->
        if (getValue progId register a) > 0
          then (prog {pos = pos + (getValue progId register b)}, Nothing)
          else (prog {pos = pos + 1}, Nothing)

binaryOp prog@(Program {progId = progId, pos = pos, register = register}) a b op =
  let aval = getValue progId register (Register a)
      bval = getValue progId register b
  in ( prog {register = M.insert a (op aval bval) register, pos = pos + 1}
     , Nothing)
