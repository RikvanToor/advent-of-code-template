module Days.Day01 where

import           Control.Applicative  ( (<|>) )
import           Data.Maybe           ( listToMaybe )
import qualified Program.RunDay as R  ( runDay )
import           Data.Attoparsec.Text

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = many' (decimal <* (endOfLine <|> endOfInput))

------------ TYPES ------------
type Input = [Int]

type OutputA = Maybe Int

type OutputB = Maybe Int

------------ PART A ------------
partA :: Input -> OutputA
partA input =
  listToMaybe [x*y | x <- input, y <- input, x+y == 2020]

------------ PART B ------------
partB :: Input -> OutputB
partB input =
  listToMaybe [x*y*z | x <- input, y <- input, z <- input, x+y+z == 2020]