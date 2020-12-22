module Days.Day22 (runDay) where

import Data.Bifunctor                        ( bimap )
import Data.Vector                           ( Vector )
import qualified Data.Vector as Vec
import qualified Data.Vector.Unboxed as UVec

import qualified Program.RunDay as R         ( runDay )
import Data.Attoparsec.Text

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser =
  bimap UVec.fromList UVec.fromList
    <$> ( (,)
            <$ string "Player 1:"
            <* endOfLine
            <*> decimal `sepBy` endOfLine
            <* endOfLine
            <* endOfLine
            <* string "Player 2:"
            <* endOfLine
            <*> decimal `sepBy` endOfLine
        )

------------ TYPES ------------
type Input = (UVec.Vector Int, UVec.Vector Int)

type OutputA = Int

type OutputB = Int

data Player = P1 | P2
  deriving (Show)

------------ PART A ------------
runA :: Input -> UVec.Vector Int
runA (p1s, p2s)
  | UVec.null p1s = p2s
  | UVec.null p2s = p1s
  | UVec.head p1s > UVec.head p2s = runA (takeCards p1s p2s, UVec.tail p2s)
  | otherwise = runA (UVec.tail p1s, takeCards p2s p1s)

appendTwo :: UVec.Unbox a => UVec.Vector a -> a -> a -> UVec.Vector a
appendTwo v a = UVec.snoc (UVec.snoc v a)

takeCards :: UVec.Vector Int -> UVec.Vector Int -> UVec.Vector Int
takeCards v1 v2 = appendTwo (UVec.tail v1) (UVec.head v1) (UVec.head v2)

partA :: Input -> OutputA
partA = sum . zipWith (*) [1 ..] . reverse . UVec.toList . runA

------------ PART B ------------
runB :: Input -> Vector Input -> (Player, UVec.Vector Int)
runB ps@(p1s, p2s) v
  | UVec.null p1s = (P2, p2s)
  | UVec.null p2s = (P1, p1s)
  | ps `elem` v = (P1, p1s)
  | let p1 = UVec.head p1s
        p2 = UVec.head p2s
     in (UVec.length p1s - 1) >= p1 && (UVec.length p2s - 1) >= p2 = case runB (UVec.take (UVec.head p1s) (UVec.tail p1s), UVec.take (UVec.head p2s) (UVec.tail p2s)) mempty of
    (P1, _) -> runB (takeCards p1s p2s, UVec.tail p2s) v2
    (P2, _) -> runB (UVec.tail p1s, takeCards p2s p1s) v2
  | UVec.head p1s > UVec.head p2s = runB (takeCards p1s p2s, UVec.tail p2s) v2
  | otherwise = runB (UVec.tail p1s, takeCards p2s p1s) v2
  where
    v2 = Vec.snoc v ps

partB :: Input -> OutputB
partB = sum . zipWith (*) [1 ..] . reverse . UVec.toList . snd . (`runB` mempty)
