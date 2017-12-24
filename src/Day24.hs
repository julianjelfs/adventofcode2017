module Day24 where

import qualified Text.Parsec as P
import qualified Common as C
import qualified Data.Set as S

type Component = (Int,Int)
type Bridge = [Component]

parse = do
  inp <- readFile "data/day24.txt"
  return $ fmap S.fromList $ C.parse (P.sepBy componentParser (P.char '\n')) inp

componentParser =
  (,) <$> (C.numberParser <* (P.char '/')) <*> C.numberParser

matches n = S.filter (\(p1, p2) -> n == p1 || n == p2)

buildBridges :: Int -> Bridge -> S.Set Component -> [Bridge]
buildBridges port bridge components =
  let ms = matches port components
  in case length ms == 0 of
      True -> [bridge]
      False -> concatMap buildMatch ms
  where buildMatch m@(p1, p2) =
          let p = (if port == p1 then p2 else p1)
          in buildBridges p (bridge ++ [m]) (S.delete m components)

strongestBridge = maximum . (fmap strength)

strength = sum . fmap (\(a,b) -> a+b)

maxlength bridges = maximum $ (fmap length bridges)

bestBridge bridges =
  let maxl = maxlength bridges
  in strongestBridge $ filter (\b -> length b == maxl) bridges

partOne = do
  components <- parse
  return $ fmap (strongestBridge . (buildBridges 0 [])) components

partTwo = do
  components <- parse
  return $ fmap (bestBridge . buildBridges 0 []) components
