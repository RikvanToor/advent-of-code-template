module Days.Day04 (runDay) where

import           Control.Applicative  ( (<|>) )
import           Data.Char            ( isDigit )
import           Data.List as List
import           Data.Map.Strict      ( Map, (!?) )
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Text.Read            ( readMaybe )

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = (Map.fromList <$> many1 field) `sepBy` endOfLine
  where field = (,) <$> many1 symb <* char ':' <*> many1 symb <* sp
        symb = letter <|> digit <|> char '#'
        sp   = () <$ char ' ' <|> endOfLine

------------ TYPES ------------
type Input = [Map String String]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA i =
  let keys = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
  in length $ filter (\m -> all (`Map.member` m) keys) i

------------ PART B ------------
checkRecord :: Map String String -> Bool
checkRecord m = fromMaybe False $ do
  byr <- m !? "byr"
  byrI <- readMaybe byr :: Maybe Int
  let byrValid = length byr == 4 && byrI >= 1920 && byrI <= 2002

  iyr <- m !? "iyr"
  iyrI <- readMaybe iyr :: Maybe Int
  let iyrValid = length iyr == 4 && iyrI >= 2010 && iyrI <= 2020

  eyr <- m !? "eyr"
  eyrI <- readMaybe eyr :: Maybe Int
  let eyrValid = length eyr == 4 && eyrI >= 2020 && eyrI <= 2030

  hgt <- m !? "hgt"
  hgtI <- readMaybe (List.takeWhile isDigit hgt) :: Maybe Int
  let hgtU = dropWhile isDigit hgt
  let hgtValid = (hgtU == "in" && hgtI >= 59 && hgtI <= 76)
        || (hgtU == "cm" && hgtI >= 150 && hgtI <= 193)

  hcl <- m !? "hcl"
  let hclValid = length hcl == 7
        && List.take 1 hcl == "#"
        && all (\x -> isDigit x || (x >= 'a' && x <= 'f')) (drop 1 hcl)

  ecl <- m !? "ecl"
  let eclValid = ecl `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

  pid <- m !? "pid"
  _ <- readMaybe pid :: Maybe Int
  let pidValid = length pid == 9

  return $ byrValid && iyrValid && eyrValid && hgtValid && hclValid && eclValid && pidValid

partB :: Input -> OutputB
partB i = length $ filter checkRecord i
