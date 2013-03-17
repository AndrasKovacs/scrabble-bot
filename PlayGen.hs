{-# LANGUAGE LambdaCase, TupleSections, RecordWildCards, BangPatterns #-}

import Data.Int                     (Int32)
import Data.Array.Unboxed           ((!), UArray, Array, listArray, array, bounds, assocs, accumArray)
import Data.Binary                  (decodeFile)
import Data.Bits                    (shiftL, shiftR, (.&.), setBit, clearBit, testBit)
import Data.List                    (find, transpose, foldl', sortBy, partition)
import Data.List.Split              (chunksOf)
import Data.Char                    (chr, ord, isLower, toUpper, toLower, isLetter)
import Data.Ix                      (range, inRange)
import Control.Arrow                ((***), first, second)
import Debug.Trace                  (traceShow)
import Control.Parallel.Strategies  (parMap, rdeepseq)
import Data.Ord                     (comparing)
import Data.Tuple                   (swap)

-- *********************** Basic types, helpers and game data *****************************

type TrieArray  = UArray Int32 Int32
type CellIndex  = (Int32, Int32)
type Prefix     = (TrieNode, String, (Int, String))

data Cell      = Filled Char | Anchor {crossScore :: !Int, lset:: !LetterSet} | Empty deriving (Eq, Show)
data Direction = V | H deriving (Eq, Show)
data Play      = Play {direction :: Direction, location :: CellIndex, score :: Int, word :: String} deriving (Eq, Show)

tableBounds = ((1,1), (15,15)) :: (CellIndex, CellIndex)
inBounds    = inRange tableBounds
outOfBounds = not . inBounds

trieFile = "twl06.dawg"

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

data Bonus = Nil | LS2 | LS3 | WS2 | WS3 deriving (Eq, Enum, Show)

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


-- ****************** LetterSet ************************************

newtype LetterSet = LSet Int32 deriving (Eq)

hasLetter (LSet x) l = testBit x (ord l - ord 'A')
setLetter (LSet x) l = LSet $ setBit x (ord l - ord 'A')
fromList = foldl' setLetter (LSet 0)
fullLSet = fromList $ wildcard:['A'..'Z']

instance Show LetterSet where 
    show x = "{LSet " ++ (filter (hasLetter x) (wildcard:['A'..'Z'])) ++  "}"


-- ***************** Trie functions *******************************

data TrieNode = TrieNode {
    {- UNPACK -} trieArray :: !TrieArray,
    {- UNPACK -} child     :: !Int32, 
    {- UNPACK -} val       :: !Char, 
    {- UNPACK -} eol       :: !Bool, 
    {- UNPACK -} eow       :: !Bool} 

instance Show TrieNode where
    show node = "{child: " ++ (show $ child node) ++
                ", val: " ++ val node:
                ", eol: " ++ (show $ eol node) ++
                ", eow: " ++ (show $ eow node) ++ "}"

getNode :: TrieArray -> Int32 -> TrieNode
getNode !t !i = let n = t ! i in
    TrieNode {trieArray = t,
              child     = shiftR (n .&. 4294966272) 10,
              val       = chr $ fromIntegral $ shiftR (n .&. 1020) 2,
              eol       = shiftR (n .&. 2) 1 == 1,
              eow       = (n .&. 1) == 1}

getRoot :: TrieArray -> TrieNode
getRoot t = getNode t (snd $ bounds t)

readTrie :: IO TrieNode
readTrie = getRoot `fmap` decodeFile trieFile

getChildren :: TrieNode -> [TrieNode]
getChildren !(TrieNode t ch _ _ _)
    | ch == 0   = []
    | otherwise = go ch [] where
        go !i !acc = if eol n then n:acc else go (i + 1) (n:acc) where
            n = getNode t i

contains :: TrieNode -> String -> Bool
contains = go where
    go !n ![]     = eow n
    go !n !(x:xs) = case find ((==toUpper x) . val) (getChildren n) of
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
parseTable :: [String] -> TrieNode -> Array CellIndex Cell 
parseTable table trie | check table = listArray tableBounds $ map go $ assocs table' 
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
                    fromUp    = reverse $ step $ goUp i
                    fromDown  = step $ goDown i
                    crsScore  = sum $ map (sum . map pieceScore) [fromUp, fromDown]
                    lset      | null (fromUp ++ fromDown) = fullLSet
                              | otherwise = fromList $ wildcard: [c | c <- ['A'..'Z'], 
                                                                      contains trie (fromUp ++ c:fromDown)]

-- **************************** play generation ***********************************************

genPlays :: Direction -> [String] -> TrieNode -> String -> [Play]
genPlays dir tbl trie rck = getScores =<< (filter (not . null . snd) . parMap rdeepseq genPlaysAt . range) tableBounds where

    table :: Array CellIndex Cell
    table = parseTable tbl trie 

    rack :: String 
    (wcardlist, rack) = partition (==wildcard) rck

    wcardnum :: Int 
    wcardnum = length wcardlist

    maybeDel :: String -> TrieNode -> Maybe (TrieNode, String)
    maybeDel []     n = Nothing
    maybeDel (x:xs) n | x == val n = Just (n, xs)
                      | otherwise  = maybeDel xs n >>= return . second (x:)

    leftParts :: [Prefix]
    leftParts = (trie, "", (wcardnum, rack)): next trie rack wcardnum where

        next n r w = no_wcard ++ with_wcard where
            chs        = getChildren n 
            no_wcard   = (if null r then [] else concat [go id ch r' w | Just (ch, r') <- map (maybeDel r) chs]) 
            with_wcard = (if w == 0 then [] else concat [go toLower ch r (w - 1) | ch <- chs])

        go f n r w = (n, [c], (w, r)): [(n, c:p, wr) | (n, p, wr) <- next n r w] where
            c = f (val n)

    leftPartMemo :: Array Int [Prefix]
    leftPartMemo = accumArray (flip (:)) [] (0, length rck) [(length pref, a) | a@(n, pref, (w, r)) <- leftParts]

    getLeftParts :: Int -> [Prefix]
    getLeftParts l = [0..l] >>= (leftPartMemo!)

    getRightParts :: CellIndex -> Prefix -> [(String, String)]
    getRightParts i (n, pref, (w, r)) = map (pref,) $ next n r w i (table ! i) where

        next n r w (i,j) (Filled c) = concat [go (const c) ch r w (i, j + 1) | ch <- getChildren n, val ch == toUpper c]
        next n r w (i,j) cell       = no_wcard ++ with_wcard where
            i'         = (i, j + 1)
            chs        = (if isEmpty cell then id else filter (hasLetter (lset cell) . val)) (getChildren n)
            no_wcard   = (if null r then [] else concat [go id ch r' w i' | Just (ch, r') <- map (maybeDel r) chs])
            with_wcard = (if w == 0 then [] else concat [go toLower ch r (w - 1) i' | ch <- chs])

        go f n r w i = add ++ rest where
            c    = f (val n)
            cell = table ! i
            inb  = inBounds i
            add  = (if eow n && ((not inb) || (not $ isFilled cell)) then [[c]] else [])
            rest = (if inb then map (c:) (next n r w i cell) else []) 

    genPlaysAt :: CellIndex -> (CellIndex, [(String, String)])
    genPlaysAt i | not $ isAnchor $ table ! i = (i, [])
                 | not (null leftWord)        = (i, getRightParts i (leftNode, leftWord, (wcardnum, rack)))
                 | otherwise                  = (i, getLeftParts prefLen >>= getRightParts i)
        where stepLeft = map (table!) $ drop 1 $ goLeft i
              leftWord = reverse $ map (\(Filled c) -> c) $ takeWhile isFilled stepLeft
              leftNode = getNodeOf trie leftWord
              prefLen  = min 7 (length $ takeWhile isEmpty $ stepLeft)

    getScores :: (CellIndex, [(String, String)]) -> [Play]
    getScores ((i, j), playwords) = map go playwords where

        go (a, b) = Play dir start' totalScore word where
            start   = (i, j - (fromIntegral $ length a))
            cells   = map (table!) (goRight start)
            bonuses = map (bonusTable!) (goRight start)
            word    = a ++ b
            start'  = (if dir == H then id else swap) start

            wordMod s = \case
                WS2 -> 2 * s
                WS3 -> 3 * s
                _   -> s 

            process (!wsc, !csc, !wmods, !bingo) (char, cell, bonus) = let
                notfill = not $ isFilled cell
                ps      = pieceScore char
                cscore  = crossScore cell
                lscore | not notfill  = ps
                       | bonus == LS2 = 2*ps
                       | bonus == LS3 = 3*ps
                       | otherwise    = ps
                csc' = csc + (if isAnchor cell && cscore /= 0
                                then wordMod (lscore + cscore) bonus
                                else 0)
                wmods' = if elem bonus [WS2, WS3] && notfill then bonus:wmods else wmods
                in (wsc + lscore, csc', wmods', bingo + fromEnum notfill)

            (wsc, csc, wmods, bingo) = foldl' process (0, 0, [], 0) (zip3 word cells bonuses)
            totalScore = csc + (foldl' wordMod wsc wmods) + (if bingo == 7 then 50 else 0)


genAllPlays :: [String] -> TrieNode -> String -> [Play]
genAllPlays table trie rack = sortBy (flip $ comparing score) plays where
    plays = zip [H, V] [table, transpose table] >>= \(d, t) -> genPlays d t trie rack


showPlay :: [String] -> Play -> [String]
showPlay table p@(Play d l s w) = let
    as = zip ((if d == H then goRight else goDown) l) w
    bs = zip (range tableBounds) (concat table)

    insert ((i, a):as) ((j, b):bs)
        | i == j    = a: insert as bs
        | otherwise = b: insert ((i, a):as) bs
    insert a b = map snd a ++ map snd b

    in show p :(chunksOf (length table) $ insert as bs)
  

main = do
    trie <- readTrie
    let solutions = genAllPlays table trie "ETAOI_C"
    putStrLn $ "Number of solutions: " ++ show (length solutions)
    mapM_ (mapM_ print) $ take 10 $ map (showPlay table) solutions




table =  [
    "          CARAT",
    "          A    ",
    "          I    ",
    "          R    ",
    "     HARDEN    ",
    "         N     ",
    "         T     ",
    "      TOUR     ",
    "         YONDER",
    "               ",
    "               ",
    "               ",
    "               ",
    "               ",
    "               "]

