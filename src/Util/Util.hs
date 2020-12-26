module Util.Util where

{- ORMOLU_DISABLE -}
import Control.Applicative ((<|>))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Attoparsec.Text hiding (take)
{- ORMOLU_ENABLE -}

{-
This module contains a series of miscellaneous utility functions that I have found helpful in the past.
-}

-- Takes a list.
-- Returns a map from elements of that list to the number of times they appeared in the list.
freq :: (Ord a) => [a] -> Map a Int
freq = Map.fromListWith (+) . fmap (,1)

-- Takes a nested list (to be thought of as a 2D structure).
-- Returns a map from "co-ordinates" to the items in the list.
-- For example:
--     Input: [[a,b,c],[d,e]]
--     Output: Map.fromList [((0,0),a), ((0,1),b), ((0,2),c), ((1,0),d), ((1,1),e)]
mapFromNestedLists :: (Ord a) => [[a]] -> Map (Int, Int) a
mapFromNestedLists = Map.fromList . attachCoords 0 0
  where
    attachCoords _ _ [] = []
    attachCoords x _ ([] : ls) = attachCoords (x + 1) 0 ls
    attachCoords x y ((l : ls) : lss) = ((x, y), l) : attachCoords x (y + 1) (ls : lss)

-- Splits a list into chunks of the specified size.
-- The final chunk may be smaller than the chunk size.
-- Chunk size must be positive.
chunksOf :: Int -> [a] -> [[a]]
chunksOf n ls
  | n <= 0 = error "Cannot split into chunks of negative length."
  | length ls < n = [ls]
  | otherwise = take n ls : chunksOf n (drop n ls)

fix :: Eq a => (a -> a) -> a -> a
fix f a = if a == f a then a else fix f (f a)

tupAdd :: Num a => (a, a) -> (a, a) -> (a, a)
tupAdd (a,b) (c,d) = (a+c,b+d)

mapParser :: (Char -> a) -> Parser (Map (Int, Int) a)
mapParser f =
  let row = zip [0..] <$> manyTill (f <$> anyChar) (endOfLine <|> endOfInput)
  in Map.fromList
    . concat
    . zipWith (\y -> map (\(x, c) -> ((x, y), c))) [0..]
    <$> manyTill row endOfInput

printMap :: (a -> Char) -> Map (Int, Int) a -> String
printMap f m =
  let maxX = maximum (map fst (Map.keys m))
      maxY = maximum (map snd (Map.keys m))
  in unlines $ map (\y -> map (\x -> maybe ' ' f $ Map.lookup (x,y) m) [0..maxX]) [0..maxY]

fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a

snd3 :: (a,b,c) -> b
snd3 (_,b,_) = b

trd3 :: (a,b,c) -> c
trd3 (_,_,c) = c

uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f (a,b,c) = f a b c