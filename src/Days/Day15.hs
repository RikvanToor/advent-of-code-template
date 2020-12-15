module Days.Day15 (runDay) where

import           Data.Map.Strict     ( Map )
import qualified Data.Map.Strict as Map
import qualified Util.Util as U

import qualified Program.RunDay as R ( runDay )
import           Data.Attoparsec.Text hiding ( take )

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = decimal `sepBy` char ','

------------ TYPES ------------
type Input = [Int]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
step :: Int -> Int -> Map Int [Int] -> (Int, Int, Map Int [Int])
step turn i m = 
  let newNum = case take 2 <$> Map.lookup i m of
        (Just [x,y]) -> x-y
        _            -> 0
  in (turn+1, newNum, Map.insertWith (\new old -> new ++ take 1 old) newNum [turn] m)

stepUntilTurn :: Int -> Int -> Int -> Map Int [Int] -> Int
stepUntilTurn maxTurn turn i m
  | turn == maxTurn + 1 = i
  | otherwise = U.uncurry3 (stepUntilTurn maxTurn) (step turn i m)

initAndRunUntil :: Int -> Input -> Int
initAndRunUntil maxTurn is =
  let m     = Map.fromList $ zipWith (flip (,)) (map pure [1..] :: [[Int]]) is
      lastN = last is
      turn  = length is + 1
  in stepUntilTurn maxTurn turn lastN m

partA :: Input -> OutputA
partA = initAndRunUntil 2020


------------ PART B ------------
partB :: Input -> OutputB
partB = initAndRunUntil 30000000
