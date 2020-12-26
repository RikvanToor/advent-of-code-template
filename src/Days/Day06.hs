module Days.Day06 (runDay) where

import Data.Set                      ( Set )
import qualified Data.Set as Set

import qualified Program.RunDay as R ( runDay )
import Data.Attoparsec.Text

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = many' g
  where l = Set.fromList <$> manyTill letter endOfLine
        g = manyTill l endOfLine

------------ TYPES ------------
type Input = [[Set Char]]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA = sum . map (length . Set.unions)

------------ PART B ------------
partB :: Input -> OutputB
partB = sum . map (length . foldr1 Set.intersection)
