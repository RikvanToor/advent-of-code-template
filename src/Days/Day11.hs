module Days.Day11 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
import Data.Void
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = U.mapParser f
  where f '.' = Floor
        f 'L' = Empty
        f '#' = Taken
        f x   = error $ "Unexpected char: " ++ [x]

------------ TYPES ------------
type Input = Map (Int, Int) Tile

data Tile = Floor
          | Empty
          | Taken
  deriving (Show, Eq)

type OutputA = Int

type OutputB = Int

------------ PART A ------------
getAdjacents :: (Int, Int) -> [(Int, Int)]
getAdjacents (x, y) = [(a, b) | a <- [x-1..x+1], b <- [y-1..y+1], a /= x || b /= y]

getAdjacentTakens :: (Int, Int) -> Input -> Int
getAdjacentTakens c i =
  length $ filter (== Taken) $ mapMaybe (`Map.lookup` i) (getAdjacents c)

step :: Input -> Input
step i = Map.mapWithKey go i
  where go c t = let nrOfTakens = getAdjacentTakens c i
          in case t of
            Floor -> Floor
            Empty -> if nrOfTakens == 0 then Taken else Empty
            Taken -> if nrOfTakens >= 4 then Empty else Taken

partA :: Input -> OutputA
partA = length . Map.filter (== Taken) . U.fix step

------------ PART B ------------
getDirections :: (Int, Int) -> Input -> [[(Int, Int)]]
getDirections (x,y) i = [northWest, north, northEast, east, southEast, south, southWest, west]
  where maxX = maximum (map fst $ Map.keys i)
        maxY = maximum (map snd $ Map.keys i)
        northWest = [(x-c, y-c) | c <- [1..min x y]]
        north     = [(x, c) | c <- reverse [0..y-1]]
        northEast = [(x+c, y-c) | c <- [1..min (maxX-x) y]]
        east      = [(c, y) | c <- [x+1..maxX]]
        southEast = [(x+c, y+c) | c <- [1..min (maxX-x) (maxY-y)]]
        south     = [(x, c) | c <- [y+1..maxY]]
        southWest = [(x-c, y+c) | c <- [1..min x (maxY-y)]]
        west      = [(c, y) | c <- reverse [0..x-1]]

getFirstSeat :: [(Int, Int)] -> Input -> Maybe Tile
getFirstSeat xs i = find (/= Floor) (mapMaybe (`Map.lookup` i) xs)

getDirectionsFirstSeats :: (Int, Int) -> Input -> [Maybe Tile]
getDirectionsFirstSeats c i = map (`getFirstSeat` i) (getDirections c i)

getDirectionsTakens :: (Int, Int) -> Input -> Int
getDirectionsTakens c =
  length . filter (== Just Taken) . getDirectionsFirstSeats c

stepB :: Input -> Input
stepB i = Map.mapWithKey go i
  where go c t = let nrOfTakens = getDirectionsTakens c i
          in case t of
            Floor -> Floor
            Empty -> if nrOfTakens == 0 then Taken else Empty
            Taken -> if nrOfTakens >= 5 then Empty else Taken

printInput :: Input -> String
printInput i = unlines $ map line [0..maxY]
  where maxX = maximum (map fst $ Map.keys i)
        maxY = maximum (map snd $ Map.keys i)
        line y = map (printTile . (`Map.lookup` i) . (,y)) [0..maxX]
        printTile (Just Floor) = '.'
        printTile (Just Empty) = 'L'
        printTile (Just Taken) = '#'
        printTile Nothing      = '☐'

partB :: Input -> OutputB
partB = length . Map.filter (== Taken) . U.fix stepB
