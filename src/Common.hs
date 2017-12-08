module Common where

import qualified Text.Parsec as P

numberParser :: P.Parsec String st Int
numberParser = read <$> (P.many P.digit)
