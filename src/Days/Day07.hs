module Days.Day07 (runDay) where

import           Control.Applicative  ( (<|>) )
import           Data.Tree
import           Data.List
import           Data.Map.Strict      ( Map )
import qualified Data.Map.Strict as Map

import qualified Program.RunDay as R ( runDay )
import           Data.Attoparsec.Text

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = many' l
  where l = Rule <$> manyTill anyChar (string " bags contain ") <*> (e <|> manyTill r (endOfLine <|> endOfInput))
        r = (,) <$ many' space <*> decimal <* many' space <*> manyTill anyChar ((string " bags" <|> string " bag") <* (string ", " <|> string "."))
        e = [] <$ string "no other bags." <* (endOfLine <|> endOfInput)

------------ TYPES ------------
type Input = [Rule]

data Rule = Rule
  { ruleColour   :: String
  , ruleContains :: [(Int, String)]
  } deriving Show

type OutputA = Int

type OutputB = Int

------------ PART A ------------
getColours :: Input -> [String] -> [String]
getColours is possibs =
  let newPossibs = map ruleColour $ filter (any ((`elem` possibs) . snd) . ruleContains) is
      total = nub $ possibs ++ newPossibs
  in if possibs == total then possibs else getColours is total

partA :: Input -> OutputA
partA rules = length (getColours rules ["shiny gold"]) - 1

------------ PART B ------------
mapToTree :: Map String [(Int, String)] -> String -> Maybe (Tree String)
mapToTree m rt = do
  rs <- Map.lookup rt m
  ns <- sequenceA $ concatMap (\(i, s) -> replicate i (mapToTree m s)) rs
  return $ Node rt ns

partB :: Input -> OutputB
partB is =
  let m  = Map.fromList (map (\r -> (ruleColour r, ruleContains r)) is)
      m' = Map.update (const (Just [])) "faded blue"
         $ Map.update (const (Just [])) "dotted black"
         $ Map.update (const (Just [(5, "faded blue"), (6, "dotted black")])) "vibrant plum"
         $ Map.update (const (Just [(3, "faded blue"), (4, "dotted black")])) "dark olive"
         m
  in maybe 0 (\x -> length x - 1) $ mapToTree m' "shiny gold"
