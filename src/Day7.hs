module Day7 where

import Data.List.Split
import Data.List
import Data.Ord

type Name = String
type Weight = Int
type Children = [Node]

data Node = Node Name Weight Children deriving Show

instance Eq Node where
    (Node name1 _ _) == (Node name2 _ _) = name1 == name2

removeCommas :: [String] -> [String]
removeCommas ws =
  fmap (filter (\c -> c /= ',')) ws

parse :: IO [Node]
parse = do
  --inp <- readFile "data/day7.txt"
  inp <- readFile "data/day7.txt"
  return $ fmap (lineToNode . removeCommas . words) $ lines inp

lineToNode :: [String] -> Node
lineToNode (name : weight : _ : children) =
  Node name (read weight :: Int) (fmap (\c -> Node c 0 []) children)
lineToNode (name : weight) =
  Node name (read (head weight) :: Int) []

isChildOf :: Node -> Node -> Bool
isChildOf (Node name _ _) (Node _ _ children) =
  elem name (fmap (\(Node name _ _) -> name) children)

findNode :: [Node] -> Node -> Node
findNode nodes node =
    head $ filter ((==) node) nodes

isRoot :: [Node] -> Node -> Bool
isRoot nodes node =
  (length $ filter (isChildOf node) nodes) == 0

getRoot nodes =
    head $ filter (isRoot nodes) nodes

partOne = do
  nodes <- parse
  return $ buildTree nodes $ getRoot nodes

partTwo = do
  nodes <- parse
  let tree = buildTree nodes $ getRoot nodes
  return $ findWonkyNode 0 tree

buildTree nodes (Node n w []) = Node n w []
buildTree nodes (Node n w children) =
    Node n w (fmap (replaceChild nodes) children)

replaceChild nodes node =
    buildTree nodes (findNode nodes node)

totalWeight :: Node -> Int
totalWeight (Node _ w children) =
    w + (sum $ (fmap totalWeight children))

findWonkyNode adjustBy (Node n w []) =
    w - adjustBy
findWonkyNode adjustBy (Node n w c) =
    let
        childWeights = zip (fmap totalWeight c) c
        (min, max) = (minimumBy (comparing fst) childWeights, maximumBy (comparing fst) childWeights)
        (minw, maxw) = (fst min, fst max)
    in
        if minw /= maxw then
            findWonkyNode (maxw - minw) (snd max)
        else
            w - adjustBy












