module Day18 where

import qualified Common as C
import qualified Data.Map.Strict as M
import qualified Text.Parsec as P
import Data.Char
import qualified Data.Vector as V
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, takeMVar, putMVar)
import Control.Concurrent.Chan (newChan, writeChan, readChan)
import Data.Foldable (for_)
import Control.Monad (forever)
import Control.Exception

data InstrType
  = Number Int
  | Register Char
  deriving Show

data Instr
  = Snd InstrType
  | Set Char InstrType
  | Add Char InstrType
  | Mul Char InstrType
  | Mod Char InstrType
  | Rcv Char
  | Jgz InstrType InstrType
  deriving Show


parse = do
  inp <- readFile "data/day18.txt"
  return $ fmap V.fromList $ C.parse (P.sepBy instrParser (P.char '\n')) inp

targetParser =
  P.choice [(Number <$> C.numberParser), (Register <$> P.anyChar)]

instrParser =
  P.choice
    [ P.try $ P.string "snd " *> (Snd <$> targetParser)
    , P.try $ P.string "set " *> (Set <$> (P.anyChar <* P.char ' ') <*> targetParser)
    , P.try $ P.string "add " *> (Add <$> (P.anyChar <* P.char ' ') <*> targetParser)
    , P.try $ P.string "mul " *> (Mul <$> (P.anyChar <* P.char ' ') <*> targetParser)
    , P.try $ P.string "mod " *> (Mod <$> (P.anyChar <* P.char ' ') <*> targetParser)
    , P.try $ P.string "rcv " *> (Rcv <$> P.anyChar)
    , P.try $ P.string "jgz " *> (Jgz <$> (targetParser <* P.char ' ') <*> targetParser)
    ]

processInstruction progId instr reg freq i
    | i < 0 || i >= (length instr) = freq
    | otherwise =
        case instr V.! i of
            Rcv n -> if (getValue progId reg (Register n)) > 0 then freq else (processInstruction progId instr reg freq (i + 1))
            Snd n -> (processInstruction progId instr reg (getValue progId reg n) (i + 1))
            Set a b -> (processInstruction progId instr (M.insert a (getValue progId reg b) reg) freq (i + 1))
            Add a b ->
                let aval = getValue progId reg (Register a)
                    bval = getValue progId reg b
                in processInstruction progId instr (M.insert a (aval + bval) reg) freq (i + 1)
            Mul a b ->
                let aval = getValue progId reg (Register a)
                    bval = getValue progId reg b
                in processInstruction progId instr (M.insert a (aval * bval) reg) freq (i + 1)
            Mod a b ->
                let aval = getValue progId reg (Register a)
                    bval = getValue progId reg b
                in processInstruction progId instr (M.insert a (aval `mod` bval) reg) freq (i + 1)
            Jgz a b ->
                if (getValue progId reg a) > 0
                then processInstruction progId instr reg freq (i + (getValue progId reg b))
                else processInstruction progId instr reg freq (i + 1)

getValue progId reg (Number n) = n
getValue progId reg (Register a) =
    case M.lookup a reg of
        Just n -> n
        Nothing -> if a == 'p' then progId else 0

partOne = do
  instr <- parse
  let reg = M.empty
  return $ fmap (\i -> processInstruction 0 i reg 0 0) instr

partTwo = do
    instr <- parse
    let p0 = (0, M.empty, 0, 0, False, False, [])
        p1 = (1, M.empty, 0, 0, False, False, [])

    return $
        fmap (runPrograms p0 p1) instr

runPrograms p0 p1 instr =
    let
        (p0', m1) = stepProgram instr p0
        (p1', m0) = stepProgram instr p1
    in
        if (finished p0' && finished p1') || deadlocked p0' p1' then
            numSent p1'
        else
            runPrograms (addToQueue p0' m0) (addToQueue p1' m1) instr

addToQueue (progId, reg, i, sent, waiting, finished, q) mmsg =
    case mmsg of
        Nothing -> (progId, reg, i, sent, waiting, finished, q)
        Just msg -> (progId, reg, i, sent, waiting, finished, q ++ [msg])


deadlocked (_, _, _, _, waiting1, _, _) (_, _, _, _, waiting2, _, _) = waiting1 && waiting2

finished (_, _, _, _, _, f, _) = f

numSent (_, _, _, n, _, _, _) = n

stepProgram instr prog@(progId, reg, i, sent, waiting, finished, queue)
    | i < 0 || i >= (length instr) = do
        ((progId, reg, i, sent, waiting, True, queue), Nothing)
    | otherwise =
        case instr V.! i of
            Rcv n ->
                case queue of
                    [] -> ((progId, reg, i, sent, True, False, queue), Nothing)
                    (x:t) -> ((progId, (M.insert n x reg), (i+1), sent, False, False, t), Nothing)
            Snd n ->
                ((progId, reg, (i+1), (sent + 1), False, False, queue), Just (getValue progId reg n))
            Set a b -> ((progId, (M.insert a (getValue progId reg b) reg), (i + 1), sent, False, False, queue), Nothing)
            Add a b ->
                binaryOp prog a b (+)
            Mul a b ->
                binaryOp prog a b (*)
            Mod a b ->
                binaryOp prog a b mod
            Jgz a b ->
                if (getValue progId reg a) > 0
                then ((progId, reg, (i + (getValue progId reg b)), sent, False, False, queue), Nothing)
                else ((progId, reg, (i + 1), sent, False, False, queue), Nothing)

binaryOp (progId, reg, i, sent, waiting, finished, queue) a b op =
    let aval = getValue progId reg (Register a)
        bval = getValue progId reg b
    in ((progId, (M.insert a (op aval bval) reg), (i + 1), sent, False, False, queue), Nothing)
