module Day18 where

import qualified Common as C
import qualified Data.Map.Strict as M
import qualified Text.Parsec as P
import Data.Char
import qualified Data.Vector as V
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, takeMVar, putMVar)
import Data.Foldable (for_)

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
  | Rcv InstrType
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
    , P.try $ P.string "rcv " *> (Rcv <$> targetParser)
    , P.try $ P.string "jgz " *> (Jgz <$> (targetParser <* P.char ' ') <*> targetParser)
    ]

processInstruction instr reg freq i
    | i < 0 || i >= (length instr) = freq
    | otherwise =
        case instr V.! i of
            Rcv n -> if (getValue reg n) > 0 then freq else (processInstruction instr reg freq (i + 1))
            Snd n -> (processInstruction instr reg (getValue reg n) (i + 1))
            Set a b -> (processInstruction instr (M.insert a (getValue reg b) reg) freq (i + 1))
            Add a b ->
                let aval = getValue reg (Register a)
                    bval = getValue reg b
                in processInstruction instr (M.insert a (aval + bval) reg) freq (i + 1)
            Mul a b ->
                let aval = getValue reg (Register a)
                    bval = getValue reg b
                in processInstruction instr (M.insert a (aval * bval) reg) freq (i + 1)
            Mod a b ->
                let aval = getValue reg (Register a)
                    bval = getValue reg b
                in processInstruction instr (M.insert a (aval `mod` bval) reg) freq (i + 1)
            Jgz a b ->
                if (getValue reg a) > 0
                then processInstruction instr reg freq (i + (getValue reg b))
                else processInstruction instr reg freq (i + 1)

getValue reg (Number n) = n
getValue reg (Register a) =
    case M.lookup a reg of
        Just n -> n
        Nothing -> 0

partOne = do
  instr <- parse
  let reg = M.empty
  return $ fmap (\i -> processInstruction i reg 0 0) instr

printMsgFrom name = for_ [1..3] printMsg
    where printMsg i = do
            sleepMs 1
            putStrLn (name ++ " number " ++ show i)

testingAsync = do
    printMsgFrom "main"

    -- Fork a new thread to do some work in the background.
    forkIO (printMsgFrom "fork")

    -- Fork another thread using an inline function!
    forkIO (do
        putStrLn "starting!"
        sleepMs 5
        putStrLn "ending!")

    -- Wait for threads to finish.
    sleepMs 10

testMVar = do
    result <- newEmptyMVar

    forkIO (do
        sleepMs 5
        putStrLn "Calculated result!"
        putMVar result 42)

    putStrLn "Waiting..."
    value <- takeMVar result
    putStrLn ("The answer is: " ++ show value)


sleepMs n = threadDelay (n * 1000)

-- tutorial here https://github.com/crabmusket/haskell-simple-concurrency/blob/master/src/tutorial.md
