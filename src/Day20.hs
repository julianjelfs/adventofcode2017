module Day20 where

import qualified Common as C
import Data.List
import Data.Ord
import qualified Text.Parsec as P

type Position = (Int, Int, Int)
type Velocity = (Int, Int, Int)
type Acceleration = (Int, Int, Int)
type Particle = (Position, Velocity, Acceleration)

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
  return $ (fmap . fmap) snd $ fmap (\p -> scanl' tick (p, 1000000000) [0 .. 1000]) particles

partTwo = do
  particles <- parse
  return $ (fmap . fmap) (length . fst) $ fmap (\p -> scanl' tick (p, 1000000000) [0 .. 1000]) particles

tick :: ([Particle], Int) -> Int -> ([Particle], Int)
tick (particles, closest) _ =
  let p = tickParticle <$> particles
      dd = dedupe $ zip [0 ..] p
  in (snd <$> dd, fst $ findClosest dd)

tickParticle :: Particle -> Particle
tickParticle ((px, py, pz), (vx, vy, vz), acc@(ax, ay, az)) =
  let vx' = vx + ax
      vy' = vy + ay
      vz' = vz + az
      px' = px + vx'
      py' = py + vy'
      pz' = pz + vz'
  in ((px', py', pz'), (vx', vy', vz'), acc)

dedupe particles =
    let sorted = sortParticlesByPos particles
        groups = groupParticles sorted
    in sortParticlesByIndex $ concat $ filter (\g -> length g == 1) groups

sortParticlesByPos = sortBy (\(i, (p, _, _)) (i1, (p1, _, _)) -> compare p p1)

sortParticlesByIndex = sortBy (comparing fst)

groupParticles = groupBy (\(i, (p, _, _)) (i1, (p1, _, _)) -> p == p1)

findClosest particles = minimumBy (comparing snd) $ distance <$> particles

distance (i, ((x, y, z), _, _)) = (i, (abs x) + (abs y) + (abs z))
