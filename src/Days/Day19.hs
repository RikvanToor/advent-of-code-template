module Days.Day19 (runDay) where

import           Control.Applicative    ( (<|>) )
import           Data.Map.Strict        ( Map )
import qualified Data.Map.Strict as Map
import Data.Maybe                       ( mapMaybe )
import Data.Text                        ( Text )
import qualified Data.Text as T

import qualified Program.RunDay as R    (runDay)
import Data.Attoparsec.Text

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = Input <$> parserMapParser <* endOfLine <* endOfLine <*> stringsParser

parserMapParser :: Parser (Map Int Parser')
parserMapParser = Map.fromList <$> p `sepBy` space
  where p = (,) <$> decimal <* string ": " <*> parserParser

parserParser :: Parser Parser'
parserParser = Parser' <$> r `sepBy` string " | "
  where r = (Ref <$> decimal <|> ConstChar <$ char '"' <*> anyChar <* char '"') `sepBy` char ' '

stringsParser :: Parser [Text]
stringsParser = takeTill isEndOfLine `sepBy` endOfLine 

------------ TYPES ------------
data Input = Input
  { parsers :: Map Int Parser'
  , strings :: [Text]
  } deriving Show

data ParserRule = Ref Int
                | ConstChar Char
  deriving Show

newtype Parser' = Parser' { rules :: [[ParserRule]] }
  deriving Show

type OutputA = Maybe Int

type OutputB = Maybe Int

------------ PART A ------------

buildParser0 :: Map Int Parser' -> Maybe (Parser Text)
buildParser0 m = do
  p0 <- Map.lookup 0 m
  buildParser p0 m

buildParser :: Parser' -> Map Int Parser' -> Maybe (Parser Text)
buildParser p m =  foldr (<|>) (fail "No options") <$> traverse go (rules p)
  where go :: [ParserRule] -> Maybe (Parser Text)
        go []       = return (fail "No options provided")
        go [pr]     = step pr
        go (pr:prs) = do
          p1 <- step pr
          p2 <- go prs
          return $ (<>) <$> p1 <*> p2
        step (Ref i)       = Map.lookup i m >>= (`buildParser` m)
        step (ConstChar c) = Just (T.pack . pure <$> char c )

partA :: Input -> OutputA
partA Input{..} = do
  p0 <- buildParser0 parsers
  return $ length $ mapMaybe (either (const Nothing) Just . parseOnly (p0 <* endOfInput)) strings

------------ PART B ------------
buildParser0B :: Map Int Parser' -> Maybe (Parser Text)
buildParser0B m = do
  p42 <- Map.lookup 42 m >>= (`buildParser` m)
  p31 <- Map.lookup 31 m >>= (`buildParser` m)
  return $ do 
    res42 <- many1 p42
    res31 <- many1 p31
    if length res42 > length res31
    then return $ T.concat res42 <> T.concat res31
    else fail "There must be more 42 matches than 31 matches"

partB :: Input -> OutputB
partB Input{..} = do
  p0 <- buildParser0B parsers
  return $ length $ mapMaybe (either (const Nothing) Just . parseOnly (p0 <* endOfInput)) strings