module Day7 where

import Data.List.Split

type Name = String
type Weight = Int
type Children = [Node]

data Node = Node Name Weight Children deriving Show

removeCommas :: [String] -> [String]
removeCommas ws =
  fmap (filter (\c -> c /= ',')) ws

parse :: IO [Node]
parse = do
  inp <- readFile "data/day7.txt"
  --inp <- readFile "data/day7test.txt"
  return $ fmap (lineToNode . removeCommas . words) $ lines inp

lineToNode :: [String] -> Node
lineToNode (name : weight : _ : children) =
  Node name (read weight :: Int) (fmap (\c -> Node c 0 []) children)
lineToNode (name : weight) =
  Node name (read (head weight) :: Int) []

--------------------------------------------
-- find the node that doesn't have a parent
-- for each node check whether it is a child of any other node
-- if it is not, it is our root
--------------------------------------------

isChildOf :: Node -> Node -> Bool
isChildOf (Node name _ _) (Node _ _ children) =
  elem name (fmap (\(Node name _ _) -> name) children)

--findParents :: [Node] -> [(Node, Maybe Node)]
--findParents nodes =
--  fmap (findParent nodes) nodes
--
--findParent :: [Node] -> Node -> (Node, Maybe Node)
--findParent nodes child =
--  case filter (\n -> isChildOf child n) nodes of
--    (x : xs) -> (child, Just x)
--    [] -> (child, Nothing)

isRoot :: [Node] -> Node -> Bool
isRoot nodes node =
  (length $ filter (\n -> isChildOf node n) nodes) == 0


solve = do
  nodes <- parse
  let h = head nodes
  return $ filter (\(n, r) -> r) $ fmap (\n -> (n, isRoot nodes n)) nodes






