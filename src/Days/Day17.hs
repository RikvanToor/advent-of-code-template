module Days.Day17 (runDay) where

import           Control.Monad       ( void )
import           Data.List           ( nub )
import           Data.Map.Strict     ( Map )
import qualified Data.Map.Strict as Map
import Data.Maybe                    ( mapMaybe )
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay )
import Data.Attoparsec.Text

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = Map.mapKeys (\(x,y) -> (x,y,0,0)) . void . Map.filter (/= Inactive) <$> U.mapParser f
  where f '.' = Inactive
        f '#' = Active
        f _   = error "Unexpected character"

------------ TYPES ------------
type Input = Map Pos ()

data Cube = Active | Inactive
  deriving (Eq)

type Pos = (Int, Int, Int, Int)

type OutputA = Int

type OutputB = Int

------------ PART A ------------
adjacents :: Pos -> [Pos]
adjacents (x,y,z,w) = [(x',y',z',w') | x' <- [x-1..x+1], y' <- [y-1..y+1], z' <- [z-1..z+1], w' <- [w-1..w+1], x /= x' || y /= y' || z /= z' || w /= w']

toCheck :: Input -> [Pos]
toCheck i = nub $ Map.keys i ++ concatMap adjacents (Map.keys i)

check :: Input -> Pos -> Input -> Input
check input pos output =
  let adjs = length $ mapMaybe (`Map.lookup` input) (adjacents pos)
  in case Map.lookup pos input of
    Nothing -> if adjs == 3 then Map.insert pos () output else output
    Just _  -> if adjs == 2 || adjs == 3 then Map.insert pos () output else output

step :: (Pos -> Bool) -> Input -> Input
step f i = foldr (check i) mempty (filter f $ toCheck i)

run :: (Pos -> Bool) -> Input -> Int -> Input
run f i n = foldr ($) i (replicate n (step f))

partA :: Input -> OutputA
partA i = length $ Map.keys $ run (\(_,_,_,w) -> w == 0) i 6

------------ PART B ------------
partB :: Input -> OutputB
partB i = length $ Map.keys $ run (const True) i 6
