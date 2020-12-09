module Days.Day09 (runDay) where

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = decimal `sepBy` space

------------ TYPES ------------
type Input = [Int]

type OutputA = Maybe Int

type OutputB = Maybe Int

------------ PART A ------------
findIncorrect :: [Int] -> [Int] -> Maybe Int
findIncorrect _      []     = Nothing
findIncorrect preAmb (x:xs) =
  if null [(a,b) | a <- preAmb, b <- preAmb, a+b == x, a /= b]
  then Just x
  else findIncorrect (reverse $ (x:) $ reverse $ drop 1 preAmb) xs

partA :: Input -> OutputA
partA = uncurry findIncorrect . splitAt 25

------------ PART B ------------
takeWhile' :: ([a] -> Bool) -> [a] -> [a]
takeWhile' f = go []
  where go buf []     = buf
        go buf (x:xs) = let newBuf = buf ++ [x]
                        in if f newBuf then go newBuf xs else buf 

findSeq :: Int -> [Int] -> Maybe [Int]
findSeq x xs =
  let ans = takeWhile' ((<= x) . sum) xs
  in if sum ans == x && length ans > 1
     then Just ans
     else case xs of
       [] -> Nothing
       (_:ys) -> findSeq x ys


partB :: Input -> OutputB
partB i = do
  inv <- partA i
  s <- findSeq inv i
  return (minimum s + maximum s)
