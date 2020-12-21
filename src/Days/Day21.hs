module Days.Day21 where

import Data.Bifunctor                   ( bimap )
import Data.List                        ( intercalate, sortOn )
import Data.Map.Strict                  ( Map )
import qualified Data.Map.Strict as Map
import Data.Set                         ( Set )
import qualified Data.Set as Set
import qualified Util.Util as U

import qualified Program.RunDay as R    ( runDay )
import Data.Attoparsec.Text

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser =
  ( bimap Set.fromList Set.fromList
      <$> ( (,)
              <$> many1 letter `sepBy` space
              <* string " (contains "
              <*> many1 letter `sepBy` string ", "
              <* char ')'
          )
  )
    `sepBy` endOfLine

------------ TYPES ------------
type Input = [(Set String, Set String)]

type OutputA = Int

type OutputB = String

------------ PART A ------------
getCandidatesPerAllergen :: Input -> Map String (Set String)
getCandidatesPerAllergen =
  fmap (foldr1 Set.intersection)
    . Map.fromListWith (<>)
    . concatMap (\(is, as) -> [(a, [is]) | a <- Set.toList as])

getUnsafeIngredients :: Input -> Set String
getUnsafeIngredients =
  Set.unions
    . Map.elems
    . getCandidatesPerAllergen

partA :: Input -> OutputA
partA i =
  let unsafe = getUnsafeIngredients i
   in sum $ map (length . (`Set.difference` unsafe) . fst) i

------------ PART B ------------
step :: Map String (Set String) -> Map String (Set String)
step m =
  let definites = Map.elems $ Map.filter ((== 1) . length) m
   in fmap (\ss -> if length ss > 1 then foldl Set.difference ss definites else ss) m

partB :: Input -> OutputB
partB i =
  intercalate "," $
    map snd $
      sortOn fst $
        map (fmap (head . Set.toList)) $
          Map.toList (U.fix step unsafe)
  where
    unsafe = getCandidatesPerAllergen i
