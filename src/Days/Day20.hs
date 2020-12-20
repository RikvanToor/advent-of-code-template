module Days.Day20 (runDay) where

import Data.Bifunctor                   ( first, second )
import Data.List                        ( find )
import Data.Map.Strict                  ( Map )
import qualified Data.Map.Strict as Map
import Data.Maybe                       ( fromMaybe, mapMaybe )

import qualified Program.RunDay as R    ( runDay )
import Data.Attoparsec.Text

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = Map.fromList <$> t `sepBy` endOfLine
  where t = (,) <$ string "Tile " <*> decimal <* char ':' <* endOfLine <*> mapParser

mapParser :: Parser (Map (Int, Int) Char)
mapParser =
  let row = zip [0..] <$> count 10 anyChar <* endOfLine
  in Map.fromList
    . concat
    . zipWith (\y -> map (\(x, c) -> ((x, y), c))) [0..]
    <$> count 10 row

------------ TYPES ------------
type Input = Map Int (Map (Int, Int) Char)

type OutputA = Int

type OutputB = Int

------------ PART A ------------
getSides :: Map (Int, Int) Char -> [String]
getSides m = [top, right, bot, left]
  where top   = mapMaybe (\x -> Map.lookup (x,0) m) [0..9]
        bot   = mapMaybe (\x -> Map.lookup (x,9) m) [0..9]
        left  = mapMaybe (\y -> Map.lookup (0,y) m) [0..9]
        right = mapMaybe (\y -> Map.lookup (9,y) m) [0..9]

findCorners :: Input -> [Int]
findCorners ts = Map.keys $ Map.filter (== 2) adjCounts
  where adjCounts = Map.mapWithKey (\tid tsides ->
          length $ Map.filterWithKey (\tid2 tsides2 -> or [t1 == t2 || t1 == reverse t2 |
           tid /= tid2, t1 <- tsides, t2 <- tsides2]) allSides)
          allSides
        allSides :: Map Int [String]
        allSides = fmap getSides ts

partA :: Input -> OutputA
partA = product . findCorners

------------ PART B ------------

rotate :: Int -> Map (Int, Int) a -> Map (Int, Int) a
rotate 0 m = m
rotate i m = let maxY = maximum $ map snd $ Map.keys m
  in rotate (i-1) (Map.mapKeys (\(x,y) -> (maxY-y,x)) m)

getFirstCornerAsTopLeft :: Input -> (Int, Map (Int, Int) Char)
getFirstCornerAsTopLeft ts =
  let cid     = head $ findCorners ts
      cm = fromMaybe (error "Impossible") (Map.lookup cid ts)
      [ctop, cright, cbot, cleft] = getSides cm
      allSides = fmap getSides (Map.delete cid ts)
      topAdj = length $ find (\tsides2 -> ctop `elem` tsides2 || reverse ctop `elem` tsides2) allSides
      rightAdj = length $ find (\tsides2 -> cright `elem` tsides2 || reverse cright `elem` tsides2) allSides
      botAdj = length $ find (\tsides2 -> cbot `elem` tsides2 || reverse cbot `elem` tsides2) allSides
      leftAdj = length $ find (\tsides2 -> cleft `elem` tsides2 || reverse cleft `elem` tsides2) allSides
      adjs = [topAdj, rightAdj, botAdj, leftAdj]
  in (cid, case adjs of
    [1,1,0,0] -> rotate 1 cm
    [0,1,1,0] -> cm
    [0,0,1,1] -> rotate 3 cm
    [1,0,0,1] -> rotate 2 cm
    _         -> error "No two adjacents"
   )

flipVertical :: Map (Int, Int) Char -> Map (Int, Int) Char
flipVertical m = let maxY = maximum $ map snd $ Map.keys m
  in Map.mapKeys (second (maxY -)) m

flipHorizontal :: Map (Int, Int) Char -> Map (Int, Int) Char
flipHorizontal m = let maxX = maximum $ map fst $ Map.keys m
  in Map.mapKeys (first (maxX -)) m

matchLeftRow :: String -> Map (Int, Int) Char -> Maybe (Map (Int, Int) Char)
matchLeftRow = go 3
  where go :: Int -> String -> Map (Int, Int) Char -> Maybe (Map (Int, Int) Char)
        go i c m =
          let left = mapMaybe (\y -> Map.lookup (0,y) m) [0..9]
          in if left == c
             then Just m
             else if reverse left == c
             then Just (flipVertical m)
             else if i > 0
             then go (i-1) c (rotate 1 m)
             else Nothing

matchTopRow :: String -> Map (Int, Int) Char -> Maybe (Map (Int, Int) Char)
matchTopRow = go 3
  where go :: Int -> String -> Map (Int, Int) Char -> Maybe (Map (Int, Int) Char)
        go i c m =
          let top = mapMaybe (\x -> Map.lookup (x, 0) m) [0..9]
          in if top == c
             then Just m
             else if reverse top == c
             then Just (flipHorizontal m)
             else if i > 0
             then go (i-1) c (rotate 1 m)
             else Nothing

buildRow :: (Int, Map (Int, Int) Char) -> Input -> [(Int, Map (Int, Int) Char)]
buildRow (tid, tm) ts = case Map.toList (Map.mapMaybe (matchLeftRow right) ts') of
  []  -> []
  [x] -> x : buildRow x ts'
  _   -> error "Multiple options possible"
  where right = mapMaybe (\y -> Map.lookup (9,y) tm) [0..9]
        ts'   = Map.delete tid ts

buildRows :: (Int, Map (Int, Int) Char) -> Input -> [[(Int, Map (Int, Int) Char)]]
buildRows topLeft@(_, tm) ts =
  let row = topLeft : buildRow topLeft ts
      ts' = foldr (Map.delete . fst) ts row
      bottom = mapMaybe (\x -> Map.lookup (x,9) tm) [0..9]
  in case Map.toList (Map.mapMaybe (matchTopRow bottom) ts') of
    [] -> [row]
    [x@(xid,_)] -> row : buildRows x (Map.delete xid ts')
    _ -> error "Multiple vertical options possible"

mergeRow :: [(Int, Map (Int, Int) Char)] -> Map (Int, Int) Char
mergeRow = foldl go mempty
  where go m (_, m2) = let fullMap = Map.filterWithKey (\(x,y) _ -> x>0 && x <9 && y>0 && y<9) m2
                           maxX = maximumOrMinusOne (map fst $ Map.keys m)
                       in Map.unionWith seq m $ Map.mapKeys (first (maxX+)) fullMap

mergeRows :: [Map (Int, Int) Char] -> Map (Int, Int) Char
mergeRows = foldl go mempty
  where go m m2 = let maxY = maximumOrMinusOne (map snd $ Map.keys m)
                  in Map.unionWith seq m $ Map.mapKeys (second (maxY+)) m2

maximumOrMinusOne :: (Num a, Ord a) => [a] -> a
maximumOrMinusOne [] = -1
maximumOrMinusOne xs = maximum xs

add :: (Int, Int) -> (Int, Int) -> (Int, Int)
add (a,b) (c,d) = (a+c, b+d)

patternPoints :: [(Int, Int)]
patternPoints =
  [ (0,0)
  , (5,0)
  , (6,0)
  , (11,0)
  , (12,0)
  , (17,0)
  , (18,0)
  , (19,0)
  , (18,-1)
  , (1,1)
  , (4,1)
  , (7,1)
  , (10,1)
  , (13,1)
  , (16,1)
  ]

isPattern :: (Int, Int) -> Map (Int, Int) Char -> Bool
isPattern p m = all ((== Just '#') . (`Map.lookup` m) . add p) patternPoints

excludePatterns :: Map (Int, Int) Char -> Map (Int, Int) Char
excludePatterns m =
  let points = Map.keys $ Map.filterWithKey (\p _ -> isPattern p m) m
      remove p m' = foldr (\p' -> Map.insert (p' `add` p) 'O') m' patternPoints
  in foldr remove m points

partB :: Input -> OutputB
partB i =
  let topLeft = getFirstCornerAsTopLeft i
      fullImage = mergeRows $ map mergeRow $ buildRows topLeft i
      flipped   = flipHorizontal fullImage
      allImages = map (`rotate` fullImage) [0..3] ++ map (`rotate` flipped) [0..3]
  in minimum $ map (length . Map.filter (== '#') . excludePatterns) allImages
