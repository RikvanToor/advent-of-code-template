module Days.Day16 (runDay) where

import           Data.List              ( transpose, isPrefixOf )
import qualified Data.Map.Strict as Map
import           Data.Maybe             ( listToMaybe )
import           Data.Set               ( Set )
import qualified Data.Set as Set
import qualified Util.Util as U

import qualified Program.RunDay as R    ( runDay )
import           Data.Attoparsec.Text

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = Input <$> rs <*> t <*> (filter (not . null) <$> ns)
  where rs = ruleParser `sepBy` endOfLine
        t  = string "\n\nyour ticket:\n" *> decimal `sepBy` char ','
        ns = string "\n\nnearby tickets:\n" *> (decimal `sepBy` char ',') `sepBy` space


ruleParser :: Parser Rule
ruleParser = Rule
  <$> manyTill anyChar (char ':')
  <* space
  <*> decimal
  <* char '-'
  <*> decimal
  <* string " or "
  <*> decimal
  <* char '-'
  <*> decimal

------------ TYPES ------------
data Input = Input
  { rules :: [Rule]
  , ticket :: [Int]
  , nearby :: [[Int]]
  } deriving Show

data Rule = Rule String Int Int Int Int
  deriving Show

type OutputA = Int

type OutputB = Int

------------ PART A ------------
isInvalid :: [Rule] -> Int -> Bool
isInvalid [] _
  = True
isInvalid (Rule _ min1 max1 min2 max2:rs) i
  = not ((i >= min1 && i <= max1) || (i >= min2 && i <= max2)) && isInvalid rs i


partA :: Input -> OutputA
partA Input{..} = sum $ map (sum . filter (isInvalid rules)) nearby

------------ PART B ------------
intToRuleNames :: [Rule] -> Int -> Set String
intToRuleNames (Rule s min1 max1 min2 max2:rs) i
  = if (i >= min1 && i <= max1) || (i >= min2 && i <= max2) then Set.insert s next else next
    where next = intToRuleNames rs i
intToRuleNames [] _ = mempty

cleanRuleNames :: [Set String] -> [Set String]
cleanRuleNames ss = map (deleteSingletons (singletons ss)) ss
  where singletons = concatMap Set.toList . filter ((== 1) . length)
        deleteSingletons sts s = if length s > 1 then foldr Set.delete s sts else s

findSingleOptions :: [Set String] -> [Set String]
findSingleOptions ss =
  map (\s -> let is = Set.intersection s singleOptions in if null is then s else is) ss
  where m = foldr (flip (foldr (\x -> Map.insertWith (+) x (1 :: Int)))) mempty ss
        singleOptions = Set.fromList $ Map.keys $ Map.filter (==1) m

partB :: Input -> OutputB
partB Input{..} =
  let valids = filter (not . any (isInvalid rules)) nearby
      possibleNames = map (foldr1 Set.intersection . map (intToRuleNames rules)) (transpose valids)
      names = map (listToMaybe . Set.toList) $ U.fix (cleanRuleNames . findSingleOptions) possibleNames
  in product $ map ((ticket !!). snd) $ filter ((== Just True) . fmap (isPrefixOf "departure") . fst) $ zip names [0..]
