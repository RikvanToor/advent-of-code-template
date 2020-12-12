module Days.Day12 (runDay) where

import           Control.Applicative  ( (<|>) )
import qualified Program.RunDay as R  ( runDay )
import           Data.Attoparsec.Text

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = c `sepBy` space
  where a =   Dir <$> (N <$ char 'N')
          <|> Dir <$> (S <$ char 'S')
          <|> Dir <$> (E <$ char 'E')
          <|> Dir <$> (W <$ char 'W')
          <|> L <$ char 'L'
          <|> R <$ char 'R'
          <|> F <$ char 'F'
        c = Command <$> a <*> decimal

------------ TYPES ------------
type Input = [Command]

data Direction = N
               | E
               | S
               | W
  deriving (Show, Enum)

data Action = Dir Direction
            | L
            | R
            | F
  deriving Show

data Command = Command
  { action :: Action
  , amount :: Int
  } deriving Show

type Pos = (Int, Int)

data State = State
  { position  :: Pos
  , direction :: Direction
  } deriving Show

data State2 = State2
  { shipPos :: Pos
  , wayPos  :: Pos
  } deriving Show

type OutputA = Int

type OutputB = Int

------------ PART A ------------
rotate :: Int -> Direction -> Direction
rotate d = let d' = d `div` 90
  in toEnum . (`mod` 4) . (+ d') . fromEnum

move :: Pos -> Direction -> Int -> Pos
move (x,y) d i = case d of
  N -> (x,y+i)
  E -> (x+i,y)
  S -> (x,y-i)
  W -> (x-i,y)

step :: Command -> State -> State
step Command{..} s@State{..} = case action of
  Dir d -> s{position = move position d amount}
  L     -> s{direction = rotate (-amount) direction}
  R     -> s{direction = rotate amount direction}
  F     -> s{position = move position direction amount}

partA :: Input -> OutputA
partA = let initState = State (0,0) E
  in (\(x,y) -> abs x + abs y) . position . foldr step initState . reverse

------------ PART B ------------
rotate2 :: Int -> Pos -> Pos
rotate2 d = go ((d `div` 90) `mod` 4)
  where go 0 s   = s
        go i (x,y) = go (i-1) (y, -x)

move2 :: Pos -> Pos -> Int -> Pos
move2 (x,y) (dx,dy) d = (x+dx*d, y+dy*d)

step2 :: Command -> State2 -> State2
step2 Command{..} s@State2{..} = case action of
  Dir d -> s{wayPos = move wayPos d amount}
  L     -> s{wayPos = rotate2 (-amount) wayPos}
  R     -> s{wayPos = rotate2 amount wayPos}
  F     -> s{shipPos = move2 shipPos wayPos amount}

partB :: Input -> OutputB
partB = let initState = State2 (0,0) (10,1)
  in (\(x,y) -> abs x + abs y) . shipPos . foldr step2 initState . reverse
