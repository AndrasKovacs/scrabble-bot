{-# LANGUAGE RecordWildCards #-}

module GameData where

import qualified Data.DAWG as D
import Data.Array.Unboxed
import Data.Int
import Data.Char
import Data.Ix
import Data.Bits
import Data.List
import Control.Arrow
import Control.DeepSeq


-- ***************** Cell data ********************

wildcard = '_'

type CellIndex  = (Int32, Int32)
type PrefixData = (D.Node, String, (Int, String))


data Cell      = Filled !Char | Anchor {crossScore :: !Int, lset:: !LetterSet} | Empty deriving (Eq, Show)
data Direction = V | H deriving (Eq, Show)
data Play      = Play {direction :: !Direction, location :: !CellIndex, score :: !Int, word :: !String} deriving (Eq, Show)

instance NFData Direction where
  rnf = const ()

instance NFData Play where
  rnf (Play d l s w) = rnf d `seq` rnf l `seq` rnf s `seq` rnf w `seq` ()
  

tableBounds = ((1,1), (15,15)) :: (CellIndex, CellIndex)
inBounds    = inRange tableBounds
outOfBounds = not . inBounds


stepIndex direction = takeWhile inBounds . iterate direction
[stepUp, stepDown, stepLeft, stepRight] = map stepIndex [pred *** id, succ *** id, id *** pred, id *** succ]


isEmpty Empty = True
isEmpty _     = False

isFilled (Filled _) = True 
isFilled _          = False

isAnchor (Anchor {..}) = True
isAnchor _             = False


-- ****************** Scoring *********************


letterScores :: UArray Int Int
letterScores = array (ord 'A', ord 'Z') $ map (first ord) $
    [('E', 1), ('A', 1),  ('I', 1), ('O', 1), ('N', 1), ('R', 1), ('T', 1), ('L', 0), ('S', 1), ('U', 1), 
    ('D', 2), ('G', 2), ('B', 3), ('C', 3), ('M', 3), ('P', 3), ('F', 4), ('H', 4), 
    ('V', 4), ('W', 4), ('Y', 4), ('K', 5), ('J', 8), ('X', 8), ('Q', 10), ('Z', 10)]


pieceScore :: Char -> Int
pieceScore c | isLower c = 0
             | otherwise = letterScores ! (ord c)


data Bonus = Nil | LS2 | LS3 | WS2 | WS3 deriving (Enum, Eq, Show)


bonusTable :: Array CellIndex Bonus
bonusTable = listArray tableBounds $ concatMap (map toEnum) $ (\x-> x ++ (reverse $ init x)) [
    [4, 0, 0, 1, 0, 0, 0, 4, 0, 0, 0, 1, 0, 0, 4],
    [0, 3, 0, 0, 0, 2, 0, 0, 0, 2, 0, 0, 0, 3, 0],
    [0, 0, 3, 0, 0, 0, 1, 0, 1, 0, 0, 0, 3, 0, 0],
    [1, 0, 0, 3, 0, 0, 0, 1, 0, 0, 0, 3, 0, 0, 1],
    [0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0],
    [0, 2, 0, 0, 0, 2, 0, 0, 0, 2, 0, 0, 0, 2, 0],
    [0, 0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0],
    [4, 0, 0, 1, 0, 0, 0, 3, 0, 0, 0, 1, 0, 0, 4]]


-- ***************** LetterSet ********************


newtype LetterSet = LSet Int32 deriving (Eq)


hasLetter (LSet x) l = testBit x (ord l - ord 'A')
setLetter (LSet x) l = LSet $ setBit x (ord l - ord 'A')
fromList = foldl' setLetter (LSet 0)
fullLSet = fromList $ wildcard:['A'..'Z']


instance Show LetterSet where 
    show x = "{LSet " ++ (filter (hasLetter x) (wildcard:['A'..'Z'])) ++  "}"


-- ************************************************


parseTable :: [String] -> D.Node -> Array CellIndex Cell 
parseTable table dawg | check table = listArray tableBounds $ map go $ assocs table' 
                      | otherwise   = error "Table must be 15x15" where

    table'       :: Array CellIndex Char
    table'       = listArray tableBounds $ concat table
    check t      = all (==15) $ length t: map length t
    neighs (i,j) = [table' ! i'| (di, dj) <- [(-1, 0), (1, 0), (0, -1), (0, 1)],
                                  let i' = (i + di, j + dj), inBounds i']

    go (i, c) | isLetter c             = Filled c
              | all (==' ') $ neighs i = Empty
              | otherwise              = Anchor crsScore lset where

        step      = takeWhile (/=' ') . map (toUpper . (table'!)) . tail
        fromUp    = reverse $ step $ stepUp i
        fromDown  = step $ stepDown i
        crsScore  = sum $ map (sum . map pieceScore) [fromUp, fromDown]
        lset      | null (fromUp ++ fromDown) = fullLSet
                  | otherwise = fromList $ wildcard: [c | c <- ['A'..'Z'], 
                                                          D.elem (fromUp ++ c:fromDown) dawg]