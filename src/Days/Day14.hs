module Days.Day14 (runDay) where

import           Control.Applicative  ( (<|>) )
import           Data.Bits
import           Data.Map.Strict      ( Map )
import qualified Data.Map.Strict as Map
import           Data.Word

import qualified Program.RunDay as R ( runDay )
import           Data.Attoparsec.Text

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = (setMask <|> setMem) `sepBy` endOfLine
  where setMask = SetMask <$ string "mask = " <*> mask
        mask = many' (X <$ char 'X' <|> One <$ char '1' <|> Zero <$ char '0')
        setMem  = SetMem <$ string "mem[" <*> decimal <* string "] = " <*> decimal

------------ TYPES ------------
type Input = [Command]

data Command = SetMask Mask
             | SetMem Word64 Word64
  deriving Show

type Mask = [MaskBit]

data MaskBit = X | One | Zero
  deriving Show

type OutputA = Word64

type OutputB = Word64

------------ PART A ------------
maskValue :: Mask -> Word64 -> Word64
maskValue m = (.|. zeroes) . (.&. ones)
  where ones   = foldr (pick True) 0 (zip [0..] $ reverse m)
        zeroes = foldr (pick False) 0 (zip [0..] $ reverse m)
        pick _ (i, One) w   = setBit w i
        pick _ (i, Zero) w  = clearBit w i
        pick True (i, X) w  = setBit w i
        pick False (i, X) w = clearBit w i

step :: Mask -> Map Word64 Word64 -> Command -> (Mask, Map Word64 Word64)
step _ mem (SetMask mask) = (mask, mem)
step mask mem (SetMem i w) = (mask, Map.insert i (maskValue mask w) mem)

partA :: Input -> OutputA
partA =
  sum
  . Map.elems
  . snd
  . foldl (uncurry step) (replicate 36 X, mempty)

------------ PART B ------------
allMasks :: Mask -> [Mask]
allMasks = foldr pick [[]]
  where pick One  ms = map (One:) ms
        pick Zero ms = map (X:) ms
        pick X ms    = map (One:) ms ++ map (Zero:) ms

step2 :: Mask -> Map Word64 Word64 -> Command -> (Mask, Map Word64 Word64)
step2 _ mem (SetMask mask) = (mask, mem)
step2 mask mem (SetMem i w) = (mask, foldr ((`Map.insert` w) . (`maskValue` i)) mem (allMasks mask))

partB :: Input -> OutputB
partB =
  sum
  . Map.elems
  . snd
  . foldl (uncurry step2) (replicate 36 Zero, mempty)
