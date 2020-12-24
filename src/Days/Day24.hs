module Days.Day24 (runDay) where

import           Control.Applicative    ( (<|>) )
import Data.List                        ( group, nub, sort )
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe                       ( mapMaybe )
import qualified Util.Util as U

import qualified Program.RunDay as R    ( runDay )
import Data.Attoparsec.Text

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = many1 d `sepBy` endOfLine
  where
    d =
      East <$ string "e"
        <|> SouthEast <$ string "se"
        <|> SouthWest <$ string "sw"
        <|> West <$ string "w"
        <|> NorthWest <$ string "nw"
        <|> NorthEast <$ string "ne"

------------ TYPES ------------
type Input = [[Dir]]

data Dir
  = East
  | SouthEast
  | SouthWest
  | West
  | NorthWest
  | NorthEast
  deriving (Show, Enum)

type Pos = (Int, Int)

data Colour
  = Black
  | White
  deriving (Show, Eq)

type OutputA = Int

type OutputB = Int

------------ PART A ------------
dirMovement :: Dir -> Pos
dirMovement East = (2, 0)
dirMovement SouthEast = (1, -1)
dirMovement SouthWest = (-1, -1)
dirMovement West = (-2, 0)
dirMovement NorthWest = (-1, 1)
dirMovement NorthEast = (1, 1)

getPos :: [Dir] -> Pos
getPos = foldr (U.tupAdd . dirMovement) (0, 0)

partA :: Input -> OutputA
partA =
  length
    . filter odd
    . map length
    . group
    . sort
    . map getPos

------------ PART B ------------
flipC :: Colour -> Colour
flipC White = Black
flipC Black = White

buildMap :: Input -> Map (Int, Int) Colour
buildMap = foldr ((\p -> Map.insertWith ((flipC .) . seq) p Black) . getPos) mempty

getAdjacents :: (Int, Int) -> [(Int, Int)]
getAdjacents p = map (U.tupAdd p . dirMovement) [East .. NorthEast]

getBlackAdjacents :: (Int, Int) -> Map (Int, Int) Colour -> Int
getBlackAdjacents p m = length $ filter (== Black) $ mapMaybe (`Map.lookup` m) (getAdjacents p)

step :: Map (Int, Int) Colour -> Map (Int, Int) Colour
step m = foldr go m toCheck
  where
    go p m' =
      let blackAdjs = getBlackAdjacents p m
          c = Map.lookup p m
       in case c of
            Just Black -> if blackAdjs == 0 || blackAdjs > 2 then Map.delete p m' else m'
            Just White -> if blackAdjs == 2 then Map.insert p Black m' else Map.delete p m'
            _ -> if blackAdjs == 2 then Map.insert p Black m' else m'
    toCheck = nub $ concatMap getAdjacents (Map.keys m) ++ Map.keys m

partB :: Input -> OutputB
partB = length . Map.filter (== Black) . (!! 100) . iterate step . buildMap
