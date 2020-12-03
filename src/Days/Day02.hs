module Days.Day02 (runDay) where

import           Control.Applicative  ( (<|>) )
import qualified Program.RunDay as R  ( runDay )
import           Data.Attoparsec.Text

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = many'
  $ Password
  <$> decimal
  <* char '-'
  <*> decimal
  <* space
  <*> anyChar
  <* char ':'
  <* space
  <*> manyTill anyChar (endOfLine <|> endOfInput)

------------ TYPES ------------
type Input = [Password]

data Password = Password
  { pwMin      :: Int
  , pwMax      :: Int
  , pwChar     :: Char
  , pwPassword :: String
  } deriving Show

type OutputA = Int

type OutputB = Int

------------ PART A ------------
count' :: Foldable t => (a -> Bool) -> t a -> Int
count' f = foldr (\a b -> if f a then b + 1 else b) 0

partA :: Input -> OutputA
partA = count'
  (\Password{..} ->
    let charCount = length (filter (== pwChar) pwPassword)
    in charCount >= pwMin && charCount <= pwMax
  )

------------ PART B ------------
partB :: Input -> OutputB
partB = count'
  (\Password{..} ->
    let l      = length pwPassword
        valid1 = pwMin <= l && pwPassword !! (pwMin-1) == pwChar
        valid2 = pwMax <= l && pwPassword !! (pwMax-1) == pwChar
    in (valid1 && not valid2) || (valid2 && not valid1)
  )
