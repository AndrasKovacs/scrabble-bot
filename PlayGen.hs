{-# LANGUAGE LambdaCase, TupleSections, RecordWildCards, BangPatterns #-}

import Data.Int                     (Int32)
import Data.Array.Unboxed           ((!), UArray, Array, listArray, array, bounds, assocs, accumArray)
import Data.Binary                  (decodeFile)
import Data.Bits                    (shiftL, shiftR, (.&.), setBit, clearBit, testBit)
import Data.List                    (find, transpose, foldl', sortBy, partition)
import Data.Char                    (chr, ord, isLower, toUpper, toLower, isLetter)
import Data.Ix                      (range, inRange)
import Control.Arrow                ((***), first, second)
import Debug.Trace                  (traceShow)
import Control.Parallel.Strategies  (parMap, rdeepseq)
import Data.Ord                     (comparing)
import Data.Tuple                   (swap)

-- *********************** Basic types, helpers and game data *****************************

type TrieArray      = UArray Int32 Int32
type CellIndex      = (Int32, Int32)
type ScrabbleTable  = Array CellIndex Cell
type Score          = Int
type Prefix         = (TrieNode, String, (Int, String))

data Cell = Filled Char | Anchor {upWord, downWord :: String, upScore, downScore :: Score, lset:: LetterSet} | Empty deriving (Show, Eq)
data Direction = V | H deriving (Show)
data Play = Play {direction :: Direction, location :: CellIndex, score :: Score, word :: String} deriving (Show)

tableBounds = ((1,1), (15,15)) :: (CellIndex, CellIndex)
inBounds    = inRange tableBounds
outOfBounds = not . inBounds

wildcard = '_'

goIndex direction = takeWhile inBounds . iterate direction
[goUp, goDown, goLeft, goRight] = map goIndex [pred *** id, succ *** id, id *** pred, id *** succ]

isEmpty Empty = True
isEmpty _     = False

isFilled (Filled _) = True 
isFilled _          = False

isAnchor (Anchor {..}) = True
isAnchor _             = False

letterScores :: UArray Int Int
letterScores = array (ord 'A', ord 'Z') $ map (first ord) $
    [('E', 1), ('A', 1),  ('I', 1), ('O', 1), ('N', 1), ('R', 1), ('T', 1), ('L', 0), ('S', 1), ('U', 1), 
    ('D', 2), ('G', 2), ('B', 3), ('C', 3), ('M', 3), ('P', 3), ('F', 4), ('H', 4), 
    ('V', 4), ('W', 4), ('Y', 4), ('K', 5), ('J', 8), ('X', 8), ('Q', 10), ('Z', 10)]

pieceScore :: Char -> Int
pieceScore c | isLower c = 0
             | otherwise = letterScores ! (ord c)

data Bonus = Nil | LS2 | LS3 | WS2 | WS3 deriving (Eq, Ord, Enum, Show)

bonusTable :: Array CellIndex Bonus
bonusTable = listArray tableBounds $ concat $ map (map toEnum) $ (\x-> x ++ (reverse $ init x)) [
    [4, 0, 0, 1, 0, 0, 0, 4, 0, 0, 0, 1, 0, 0, 4],
    [0, 3, 0, 0, 0, 2, 0, 0, 0, 2, 0, 0, 0, 3, 0],
    [0, 0, 3, 0, 0, 0, 1, 0, 1, 0, 0, 0, 3, 0, 0],
    [1, 0, 0, 3, 0, 0, 0, 1, 0, 0, 0, 3, 0, 0, 1],
    [0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0],
    [0, 2, 0, 0, 0, 2, 0, 0, 0, 2, 0, 0, 0, 2, 0],
    [0, 0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0],
    [4, 0, 0, 1, 0, 0, 0, 3, 0, 0, 0, 1, 0, 0, 4]]


-- ****************** LetterSet ************************************

newtype LetterSet = LSet Int32 deriving (Eq)

hasLetter (LSet x) l = testBit x (ord l - ord 'A')
setLetter (LSet x) l = LSet $ setBit x (ord l - ord 'A')
delLetter (LSet x) l = LSet $ clearBit x (ord l - ord 'A')
fromList = foldl' setLetter (LSet 0)
fullLSet = fromList $ wildcard:['A'..'Z']

instance Show LetterSet where 
    show x = "{LSet " ++ (filter (hasLetter x) (wildcard:['A'..'Z'])) ++  "}"


-- ***************** Trie functions *******************************

data TrieNode = TrieNode {
    trieArray :: !TrieArray,
    index     :: !Int32,
    children  :: !Int32, 
    val       :: !Char, 
    eol       :: !Bool, 
    eow       :: !Bool} 

instance Show TrieNode where
    show node = "{index: " ++ (show $ index node) ++
                ", children: " ++ (show $ children node) ++
                ", val: " ++ val node:
                ", eol: " ++ (show $ eol node) ++
                ", eow: " ++ (show $ eow node) ++ "}"

getNode :: TrieNode -> Int32 -> TrieNode
getNode node i = 
    TrieNode {trieArray = t,
              index     = i,
              children  = shiftR (n .&. 4294966272) 10,
              val       = chr $ fromIntegral $ shiftR (n .&. 1020) 2,
              eol       = shiftR (n .&. 2) 1 == 1,
              eow       = (n .&. 1) == 1}
    where t = trieArray node
          n = t ! i

getRoot :: TrieArray -> TrieNode
getRoot trie = getNode (TrieNode trie 0 0 '0' False False) (snd $ bounds trie)

readTrie :: IO TrieNode
readTrie = getRoot `fmap` decodeFile "trie.hsdat"

getChildren :: TrieNode -> [TrieNode]
getChildren node@(TrieNode _ i ch _ _ _)
    | i == ch   = []
    | otherwise = go child where
        child = getNode node ch
        go !n = if eol n then [n] else n:go (getNode n (index n + 1))

contains :: TrieNode -> String -> Bool
contains = go where
    go n []     = eow n
    go n (x:xs) = case find ((==toUpper x) . val) (getChildren n) of
        Nothing -> False
        Just n' -> go n' xs

getNodeOf :: TrieNode -> String -> TrieNode
getNodeOf n word = go n word where
    go n [] = n
    go n (x:xs) = case find ((==toUpper x) . val) (getChildren n) of
        Just n' -> go n' xs
        Nothing -> error $ "The Scrabble table contains an invalid word: " ++ word

{- Parse format: 
     - uppercase letters: normal pieces
     - lowercase letters: wildcards assumed to be a certain letter
     - space: empty cell -}
parseTable :: [String] -> TrieNode -> ScrabbleTable 
parseTable table trie | check table = listArray tableBounds $ map go $ assocs table' 
                      | otherwise   = error "Table must be 15x15" where

    table'       :: Array CellIndex Char
    table'       = listArray tableBounds $ concat table
    check t      = all (==15) $ length t: map length t
    neighs (i,j) = [table' ! i'| (di, dj) <- [(-1, 0), (1, 0), (0, -1), (0, 1)],
                                  let i' = (i + di, j + dj), inBounds i']

    go (i, c) | isLetter c             = Filled c
              | all (==' ') $ neighs i = Empty
              | otherwise              = Anchor fromUp fromDown upScore downScore lset where
                    step      = takeWhile (/=' ') . map (toUpper . (table'!)) . tail
                    fromUp    = reverse $ step $ goUp i
                    fromDown  = step $ goDown i
                    upScore   = sum $ map pieceScore fromUp
                    downScore = sum $ map pieceScore fromDown
                    lset      | null (fromUp ++ fromDown) = fullLSet
                              | otherwise = fromList $ wildcard: [c | c <- ['A'..'Z'], 
                                                                      contains trie (fromUp ++ c:fromDown)]

-- **************************** play generation ***********************************************

genPlays :: [String] -> TrieNode -> String -> [(CellIndex, Score, String)]
genPlays tbl trie rck = concatMap attachScore . filter (not.null.snd) . parMap rdeepseq genPlaysAt $ range tableBounds where

    table :: ScrabbleTable
    table = parseTable tbl trie 

    rack :: String 
    (wcardlist, rack) = partition (==wildcard) rck

    wcardnum :: Int 
    wcardnum = length wcardlist

    maybeDel :: String -> TrieNode -> Maybe (TrieNode, String)
    maybeDel []     n = Nothing
    maybeDel (x:xs) n | x == val n = Just (n, xs)
                      | otherwise  = maybeDel xs n >>= return . second (x:)

    genPref :: Int -> [Prefix]
    genPref l = (trie, "", (wcardnum, rack)): (if l == 0 then [] else next trie rack l wcardnum) where

        next n r l w = no_wcard ++ with_wcard where
            chs        = getChildren n 
            no_wcard   = (if null r then [] else concat [go id ch r' (l - 1) w | Just (ch, r') <- map (maybeDel r) chs]) 
            with_wcard = (if w == 0 then [] else concat [go toLower ch r (l - 1) (w - 1) | ch <- chs])

        go f n r l w = (n, [v], (w, r)): rest where
            v    = f (val n)
            rest = (if l == 0 then [] else map (\(a, b, c) -> (a, v:b, c)) (next n r l w))

    prefMemo :: Array Int [Prefix]
    prefMemo = accumArray (flip (:)) [] (0, 7) [(length pref, a) | a@(n, pref, (w, r)) <- genPref 7]

    prefixes :: Int -> [Prefix]
    prefixes maxlen = [0..maxlen] >>= (prefMemo!)

    suffixes :: CellIndex -> Prefix -> [(String, String)]
    suffixes i (n, prefix, (w, r)) = map (prefix,) $ next n r w i (table ! i) where

        next n r w (i,j) (Filled c) = concat [go (const c) ch r w (i, j + 1) | ch <- getChildren n, val ch == toUpper c]
        next n r w (i,j) cell       = no_wcard ++ with_wcard where
            i'         = (i, j + 1)
            chs        = (if isEmpty cell then id else filter (hasLetter (lset cell) . val)) (getChildren n)
            no_wcard   = (if null r then [] else concat [go id ch r' w i' | Just (ch, r') <- map (maybeDel r) chs])
            with_wcard = (if w == 0 then [] else concat [go toLower ch r (w - 1) i' | ch <- chs])

        go f n r w i = new ++ rest where
            v    = f (val n)
            cell = table ! i
            inb  = inBounds i
            new  = (if eow n && ((not inb) || (not $ isFilled cell)) then [[v]] else [])
            rest = (if inb then map (v:) (next n r w i cell) else []) 

    genPlaysAt :: CellIndex -> (CellIndex, [(String, String)])
    genPlaysAt i | not $ isAnchor $ table ! i = (i, [])
                 | not (null leftWord)        = (i, suffixes i (leftNode, leftWord, (wcardnum, rack)))
                 | otherwise                  = (i, prefixes prefLen >>= suffixes i)
        where stepLeft = map (table!) $ drop 1 $ goLeft i
              leftWord = reverse $ map (\(Filled c) -> c) $ takeWhile isFilled stepLeft
              leftNode = getNodeOf trie leftWord
              prefLen  = min 7 (length $ takeWhile isEmpty $ stepLeft)

    attachScore :: (CellIndex, [(String, String)]) -> [(CellIndex, Score, String)]
    attachScore ((i, j), playwords) = map go playwords where

        go (a, b) = (start, totalScore, word) where
            start   = (i, j - (fromIntegral $ length a))
            cells   = map (table!) (goRight start)
            bonuses = map (bonusTable!) (goRight start)
            word    = a ++ b

            wordMod s = \case
                WS2 -> 2 * s
                WS3 -> 3 * s
                _   -> s 

            process (!wsc, !csc, !wmods, !bingo) (char, cell, bonus) = let
                notfill = not $ isFilled cell
                lscore = let ps = pieceScore char in case (cell, bonus) of
                    (Filled _, _) -> ps
                    (_,      LS2) -> 2 * ps
                    (_,      LS3) -> 3 * ps
                    _             -> ps
                csc' = case cell of
                    Anchor {..} -> case upScore + downScore of
                                        0 -> csc
                                        n -> csc + (wordMod (lscore + n) bonus) 
                    _           -> csc
                wmods' = if elem bonus [WS2, WS3] && notfill
                    then bonus:wmods
                    else wmods
                in (wsc + lscore, csc', wmods', bingo + fromEnum notfill)

            (wsc, csc, wmods, bingo) = foldl' process (0, 0, [], 0) (zip3 word cells bonuses)
            totalScore = csc + (foldl' wordMod wsc wmods) + (if bingo == 7 then 50 else 0)


generateAll :: [String] -> TrieNode -> String -> [Play]
generateAll table trie rack = let
    [a, b] = map (\t -> genPlays t trie rack) [table, transpose table]
    horizontal = [Play H i scr wrd | (i, scr, wrd) <- a]
    vertical   = [Play V (swap i) scr wrd | (i, scr, wrd) <- b]
    in sortBy (flip $ comparing score) (horizontal ++ vertical)


main = do
    trie <- readTrie
    let solutions = generateAll table trie "QR"
    print $ length solutions
    mapM_ print $ take 10 $ solutions


table =  [
    "             AT",
    "              O",
    "               ",
    "               ",
    "               ",
    "               ",
    "               ",
    "               ",
    "               ",
    "               ",
    "               ",
    "               ",
    "               ",
    "               ",
    "               "]

--table =  [
--    "               ",
--    "               ",
--    "               ",
--    "               ",
--    "    CONUNDRUMS ",
--    "       N       ",
--    "       D       ",
--    "       E       ",
--    "       R       ",
--    "       D       ",
--    "       O       ",
--    "       G       ",
--    "       S       ",
--    "               ",
--    "               "]

