module Days.Day23  where

import Control.Monad.ST                  ( ST )
import qualified Data.Array.ST as A
import qualified Data.Array.Unboxed as A

import qualified Program.RunDay as R     ( runDay )
import Data.Attoparsec.Text

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = map (read . pure) <$> many' digit

------------ TYPES ------------
type Input = [Int]

type OutputA = Int

type OutputB = Maybe Int

------------ PART A ------------
setup :: [Int] -> ST s (A.STUArray s Int Int)
setup input = do
  let size = length input
  arr <-  A.newArray (0, size) 0
  let inserts []     = return ()
      inserts [x]    =  A.writeArray arr x (head input)
      inserts (x:xs) =  A.writeArray arr x (head xs) >> inserts xs
  inserts input
  A.writeArray arr 0 (head input)
  return arr

step :: (Int, A.STUArray s Int Int) -> ST s (Int, A.STUArray s Int Int)
step (i, arr) = do
  p1 <-  A.readArray arr i
  p2 <-  A.readArray arr p1
  p3 <-  A.readArray arr p2
  size <- snd <$> A.getBounds arr
  let destination = head [x | x <- [if y <= 0 then size + y else y | y <- reverse [i-4..i-1]]
                            , x /= p1 && x /= p2 && x /= p3 ]
  next <- A.readArray arr destination
  afterP3 <- A.readArray arr p3
  A.writeArray arr i afterP3
  A.writeArray arr destination p1
  A.writeArray arr p3 next
  return (afterP3, arr)

getAfter1s :: A.UArray Int Int -> Int
getAfter1s arr =
  let (_, l) = iterate (\(x, xs) -> let y = arr A.! x in (y, y:xs)) (1, []) !! 8
  in foldr (\x y -> y * 10 + x) 0 l

iterateM :: Monad m => Int -> (a -> m a) -> a -> m a
iterateM 1 f a = f a
iterateM n f a = f a >>= iterateM (n-1) f

partA :: Input -> OutputA
partA input =
  let arr = A.runSTUArray $ do
        arr' <- setup input
        _ <- iterateM 100 step (head input, arr')
        return arr'
  in getAfter1s arr

------------ PART B ------------
partB :: Input -> OutputB
partB input =
  let arr = A.runSTUArray $ do
        arr' <- setup (input ++ [10..1000000])
        _ <- iterateM 10000000 step (head input, arr')
        return arr'
      p1 = arr A.! 1
      p2 = arr A.! p1
  in Just (p1*p2)
