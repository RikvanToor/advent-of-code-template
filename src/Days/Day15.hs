module Days.Day15 (runDay) where

import           Control.Monad       ( forM_ )
import           Control.Monad.ST    ( ST, runST )
import           Data.Foldable       ( foldlM )
import qualified Data.Vector.Unboxed.Mutable as VM

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
partA :: Input -> OutputA
partA = run 2020

------------ PART B ------------
partB :: Input -> OutputB
partB = run 30000000

run :: Int -> [Int] -> Int
run target input = runST $ do
  v <- VM.replicate target 0
  forM_ (zip (init input) [1..]) $ uncurry (VM.write v)
  let len = length input
  foldlM (speakNum v) (last input) [len..target-1]

speakNum :: VM.MVector s Int -> Int -> Int -> ST s Int
speakNum v prev i = do
  prevPos <- VM.read v prev
  VM.write v prev i
  return $ if prevPos == 0 then 0 else i - prevPos
