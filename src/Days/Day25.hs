module Days.Day25 (runDay) where

import Data.List                     ( elemIndex )
import qualified Program.RunDay as R ( runDay )
import Data.Attoparsec.Text

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = (,) <$> decimal <* space <*> decimal

------------ TYPES ------------
type Input = (Int, Int)

type OutputA = Maybe Int

type OutputB = String

------------ PART A ------------
transform :: Int -> Int -> Int
transform subj val = (val*subj) `mod` 20201227

partA :: Input -> OutputA
partA (pubK1,pubK2) = do
  loop1 <- elemIndex pubK1 $ iterate (transform 7) 1
  return $ iterate (transform pubK2) 1 !! loop1

------------ PART B ------------
partB :: Input -> OutputB
partB = const "DONE!"
