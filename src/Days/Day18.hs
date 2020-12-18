module Days.Day18 (runDay) where

import           Control.Applicative  ( (<|>) )
import           Data.List            ( elemIndices, findIndices )
import           Data.Maybe           ( listToMaybe )

import qualified Program.RunDay as R  (runDay)
import Data.Attoparsec.Text hiding ( take )

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = symbsParser `sepBy` endOfLine

symbsParser :: Parser [Symb]
symbsParser = symb
  where symb = (num <|> plus <|> times <|> parens) `sepBy` char ' '
        num = Num' <$> decimal
        plus = Plus' <$ char '+'
        times = Times' <$ char '*'
        parens = SubExp <$ char '(' <*> symb <* char ')'

processSymbs :: [Symb] -> Either String Expr
processSymbs ss =
  case listToMaybe $ reverse $ findIndices (\s -> s == Plus' || s == Times') ss of
    Just x -> do
      a <- processSymbs (take x ss)
      b <- processSymbs (drop (x+1) ss)
      case ss !! x of
        Plus'  -> return $ Plus a b
        Times' -> return $ Times a b
        _      -> Left "Not an operator"
    Nothing -> case ss of
      [Num' i]     -> return $ Num i
      [SubExp ss'] -> processSymbs ss'
      _ -> Left "Multiple symbs left, but no operator in between"

processSymbs2 :: [Symb] -> Either String Expr
processSymbs2 ss =
  case listToMaybe $ reverse $ elemIndices Times' ss of
    Just x -> do
      a <- processSymbs2 (take x ss)
      b <- processSymbs2 (drop (x+1) ss)
      case ss !! x of
        Times'  -> return $ Times a b
        _      -> Left "Not an operator"
    Nothing -> 
      case listToMaybe $ reverse $ elemIndices Plus' ss of
        Just x -> do
          a <- processSymbs2 (take x ss)
          b <- processSymbs2 (drop (x+1) ss)
          case ss !! x of
            Plus'  -> return $ Plus a b
            _      -> Left "Not an operator"
        Nothing ->
          case ss of
            [Num' i]     -> return $ Num i
            [SubExp ss'] -> processSymbs2 ss'
            _ -> Left "Multiple symbs left, but no operator in between"

------------ TYPES ------------
type Input = [[Symb]]

data Symb = Num' Int
          | Plus'
          | Times'
          | SubExp [Symb]
  deriving (Show, Eq)

data Expr = Num Int
          | Plus Expr Expr
          | Times Expr Expr
  deriving Show

type OutputA = Either String Int

type OutputB = Either String Int

------------ PART A ------------
eval :: Expr -> Int
eval (Num i)     = i
eval (Plus a b)  = eval a + eval b
eval (Times a b) = eval a * eval b

partA :: Input -> OutputA
partA = fmap (sum . map eval) . traverse processSymbs . filter (not . null)

------------ PART B ------------
partB :: Input -> OutputB
partB = fmap (sum . map eval) . traverse processSymbs2 . filter (not . null)
