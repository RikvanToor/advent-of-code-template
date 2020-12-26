module Days.Day05 (runDay) where

import           Control.Applicative  ( (<|>) )
import           Data.List            ( find )
import qualified Program.RunDay as R  ( runDay )
import           Data.Attoparsec.Text

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser =
  let p1 = 0 <$ char 'F' <|> 1 <$ char 'B'
      p2 = 0 <$ char 'L' <|> 1 <$ char 'R'
      p  = bitsToInt <$> many' (p1 <|> p2)
  in p `sepBy` space

bitsToInt :: [Int] -> Int
bitsToInt = foldl (\xs x -> x + 2 * xs) 0

------------ TYPES ------------
type Input = [Int]

data Pass = Pass [Bit] [Bit]
  deriving Show

data Bit = One | Zero
  deriving Show

type OutputA = Int

type OutputB = Maybe Int

------------ PART A ------------
partA :: Input -> OutputA
partA = maximum

------------ PART B ------------
partB :: Input -> OutputB
partB ids =
  let allOpts = [minimum ids..maximum ids]
  in find (not . (`elem` ids)) allOpts
