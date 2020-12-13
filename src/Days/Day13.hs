module Days.Day13 (runDay) where

import           Control.Applicative  ( (<|>) )
import           Control.Monad        ( join )
import           Data.List
import           Data.Maybe

import qualified Program.RunDay as R (runDay)
import           Data.Attoparsec.Text

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = (,) <$> decimal <* space <*> (b `sepBy` char ',')
  where b = Just <$> decimal <|> Nothing <$ char 'x'

------------ TYPES ------------
type Input = (Int, [Maybe Int])

type OutputA = Maybe Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA (start, mbs) =
  fmap (\(bid, time) -> bid * (time-start))
  $ join
  $ find isJust
  $ map (\x -> listToMaybe
      $ mapMaybe (\b -> if x `mod` b == 0 then Just (b,x) else Nothing ) bs)
    [start..start+maxB]
  where bs = catMaybes mbs
        maxB = maximum bs

------------ PART B ------------
findTime :: (Int, Int, Int) -> (Int, Int) -> (Int, Int, Int)
findTime (offset, x, 0) (y, d)
  = (, x*y, 0)
  $ fromMaybe (error "No time found")
  $ find ((== ((y-d) `mod` y)) . (`mod` y)) (map ((+offset) . (*x)) [0..])
findTime _ _
  = error "Haven't figured out how to do this, but it should not be necessary"


partB :: Input -> OutputB
partB (_, mbs) = (\(x,_,_) -> x) $ foldr (flip findTime) (0,fst $ head bs,0) (reverse $ tail bs)
  where bs = onlyJusts $ zip mbs [0..]
        onlyJusts []               = []
        onlyJusts ((Just x, y):xs) = (x,y) : onlyJusts xs
        onlyJusts ((Nothing,_):xs) = onlyJusts xs
