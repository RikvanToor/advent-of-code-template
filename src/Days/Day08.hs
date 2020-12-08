module Days.Day08 where

import           Control.Applicative  ( (<|>) )
import           Data.Map.Strict      ( Map )
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Set             ( Set )
import qualified Data.Set as Set

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = Map.fromList . zip [0..] <$> i `sepBy` endOfLine
  where o = Acc <$ string "acc" <|> Jmp <$ string "jmp" <|> Nop <$ string "nop"
        i = Instruction <$> o <* space <*> signed decimal
        

------------ TYPES ------------
type Input = Map Int Instruction

data Operation
  = Acc
  | Jmp
  | Nop
  deriving Show

data Instruction = Instruction { op :: Operation, arg :: Int }
  deriving Show

type OutputA = Int

type OutputB = Maybe Int

------------ PART A ------------
run :: Input -> Int -> Int -> Set Int -> (Bool, Int)
run i index accum visited
  | index `elem` visited = (True, accum)
  | otherwise =
    case Map.lookup index i of
      Just (Instruction o a) -> 
        let newVisited = Set.insert index visited
        in case o of
          Nop -> run i (index + 1) accum newVisited
          Acc -> run i (index + 1) (accum + a) newVisited
          Jmp -> run i (index + a) accum newVisited
      Nothing -> (False, accum)

partA :: Input -> OutputA
partA i = snd $ run i 0 0 mempty

------------ PART B ------------
isJmpOrNop :: Instruction -> Bool
isJmpOrNop (Instruction Jmp _) = True
isJmpOrNop (Instruction Nop _) = True
isJmpOrNop _                   = False

invertJmpOrNop :: Instruction -> Instruction
invertJmpOrNop (Instruction Jmp a) = Instruction Nop a
invertJmpOrNop (Instruction Nop a) = Instruction Jmp a
invertJmpOrNop x                   = x

partB :: Input -> OutputB
partB i =
  let jns  = Map.keys (Map.filter isJmpOrNop i)
      runs = map (\jni -> run (Map.update (Just . invertJmpOrNop) jni i) 0 0 mempty) jns
  in snd <$> listToMaybe (filter (not . fst) runs)
