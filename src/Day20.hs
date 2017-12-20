module Day20 where

import qualified Common as C
import Data.List
import Data.Ord
import qualified Text.Parsec as P
import Data.Monoid

type Vector = (Int, Int, Int)
type Particle = (Vector, Vector, Vector)

parse = do
  inp <- readFile "data/day20.txt"
  return $ C.parse (P.sepBy particleParser (P.char '\n')) inp

particleParser = tuplify <$> P.sepBy partParser (P.string ", ")

partParser =
  P.choice
    [ P.string "p=" *> (tuplify <$> vecParser)
    , P.string "v=" *> (tuplify <$> vecParser)
    , P.string "a=" *> (tuplify <$> vecParser)
    ]

tuplify (x:y:z:[]) = (x, y, z)
tuplify _ = error "unexpected format"

vecParser =
  P.between (P.char '<') (P.char '>') (P.sepBy C.numberParser (P.char ','))

partOne = do
  particles <- parse
  return $
    (fmap . fmap) snd $
    fmap (\p -> scanl' tick (p, 1000000000) [0 .. 1000]) particles

partTwo = do
  particles <- parse
  return $
    (fmap . fmap) (length . fst) $
    fmap (\p -> scanl' tick (p, 1000000000) [0 .. 1000]) particles

tick :: ([Particle], Int) -> Int -> ([Particle], Int)
tick (particles, closest) _ =
  let dd = tickAndDedupe particles
  in (snd <$> dd, fst $ findClosest dd)
  where tickAndDedupe = dedupe . (zip [0..]) . (fmap tickParticle)

tickParticle :: Particle -> Particle
tickParticle (p@(px, py, pz), v@(vx, vy, vz), a@(ax, ay, az)) =
  let v1 = tuple $ sum v <> sum a
      p1 = tuple $ sum p <> sum v1
  in (p1, v1, a)
  where sum (x,y,z) = (Sum x, Sum y, Sum z)
        tuple (Sum x, Sum y, Sum z) = (x, y, z)

dedupe =
  (sortBy byIndex) . concat . (filter single) . (groupBy groupPos) . (sortBy comparePos)
  where
    comparePos (_, (p, _, _)) (_, (p1, _, _)) = compare p p1
    groupPos (_, (p, _, _)) (_, (p1, _, _)) = p == p1
    byIndex = comparing fst
    single g = length g == 1

findClosest particles =
  minimumBy (comparing snd) $ distance <$> particles

distance (i, ((x, y, z), _, _)) = (i, (abs x) + (abs y) + (abs z))
