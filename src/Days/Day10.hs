module Days.Day10 (runDay) where

import Data.List  ( sortOn, sort )
import Data.Maybe ( fromMaybe, listToMaybe )
import Data.Ord   ( Down(..) )

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text hiding (takeWhile, take)

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = decimal `sepBy` space

------------ TYPES ------------
type Input = [Int]

data Accum = Accum
  { ones   :: Int
  , twos   :: Int
  , threes :: Int
  }

type OutputA = Int

type OutputB = Int

------------ PART A ------------
bumpAccum :: Int -> Int -> Accum -> Accum
bumpAccum x y a@Accum{..} = case abs (x - y) of
  1 -> a{ones = ones + 1}
  2 -> a{twos = twos + 1}
  3 -> a{threes = threes + 1}
  _ -> a

partA :: Input -> OutputA
partA as =
  let builtin = maximum as + 3
      as'     = builtin : sortOn Down as
      res     = snd $ foldr (\x (y, accum) -> (x, bumpAccum x y accum)) (0, Accum 0 0 0) as'
  in ones res * threes res


------------ PART B ------------
countPaths :: [Int] -> Int
countPaths = fromMaybe 0 . listToMaybe . snd . foldr go ([], [])
  where go x (xs, res) =
          let options = length $ takeWhile (<= x+3) xs
          in (x:xs, max 1 (sum (take options res)) : res)

partB :: Input -> OutputB
partB = countPaths . (0:) . sort