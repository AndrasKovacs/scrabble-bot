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
import Control.Parallel.Strategies  (parMap, rdeepseq)
import Data.Ord                     (comparing)
import Data.Tuple                   (swap)
import Text.Printf                  (printf)


-- config

trieFile = "twl06.dawg"
wildcard = '_'
maxRackSize = 7
maxWcards = 2


-- Basic types, helpers and game data 

type TrieArray  = UArray Int32 Int32
type CellIndex  = (Int32, Int32)
type PrefixData = (TrieNode, String, (Int, String))

data Cell      = Filled !Char | Anchor {crossScore :: !Int, lset:: !LetterSet} | Empty deriving (Eq, Show)
data Direction = V | H deriving (Eq, Show)
data Play      = Play {direction :: !Direction, location :: !CellIndex, score :: !Int, word :: !String} deriving (Eq, Show)

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

newtype LetterSet = LSet Int32 deriving (Eq)

hasLetter (LSet x) l = testBit x (ord l - ord 'A')
setLetter (LSet x) l = LSet $ setBit x (ord l - ord 'A')
fromList = foldl' setLetter (LSet 0)
fullLSet = fromList $ wildcard:['A'..'Z']

instance Show LetterSet where 
    show x = "{LSet " ++ (filter (hasLetter x) (wildcard:['A'..'Z'])) ++  "}"


-- Trie traversal functions and data

data TrieNode = TrieNode {
    {- UNPACK -} trieArray :: !TrieArray,
    {- UNPACK -} child     :: !Int32, 
    {- UNPACK -} val       :: !Char, 
    {- UNPACK -} eol       :: !Bool, 
    {- UNPACK -} eow       :: !Bool} 

instance Show TrieNode where
    show (TrieNode {..}) = printf "{child: %d, val: %c, eol: %s, eow: %s}" child val (show eol) (show eow)

getNode :: TrieArray -> Int32 -> TrieNode
getNode !t !i = let n = t ! i in
    TrieNode {trieArray = t,
              child     = shiftR (n .&. 4294966272) 10,
              val       = chr $ fromIntegral $ shiftR (n .&. 1020) 2,
              eol       = shiftR (n .&. 2) 1 == 1,
              eow       = (n .&. 1) == 1}

readTrie :: IO TrieNode
readTrie = getRoot `fmap` decodeFile trieFile where
    getRoot t = getNode t (snd $ bounds t)

getChildren :: TrieNode -> [TrieNode]
getChildren !(TrieNode t ch _ _ _)
    | ch == 0   = []
    | otherwise = go ch [] where
        go !i !acc = if eol n then n:acc else go (i + 1) (n:acc) where
            n = getNode t i

contains :: TrieNode -> String -> Bool
contains n w = go w n where
    go ![]     !n = eow n
    go !(x:xs) !n = maybe False (go xs) (find ((==toUpper x) . val) (getChildren n))

getNodeOfWord :: TrieNode -> String -> TrieNode
getNodeOfWord n w = go w n where
    go []     n = n
    go (x:xs) n = maybe 
        (error $ "The Scrabble table contains an invalid word: " ++ w)
        (go xs) 
        (find ((==toUpper x) . val) (getChildren n))

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
        fromUp    = reverse $ step $ stepUp i
        fromDown  = step $ stepDown i
        crsScore  = sum $ map (sum . map pieceScore) [fromUp, fromDown]
        lset      | null (fromUp ++ fromDown) = fullLSet
                  | otherwise = fromList $ wildcard: [c | c <- ['A'..'Z'], 
                                                          contains trie (fromUp ++ c:fromDown)]


-- Play generation

genPlays :: Direction -> [String] -> TrieNode -> String -> [Play]
genPlays dir tbl trie rck | length rck > maxRackSize = error $ printf "Rack too big, limit is %d" maxRackSize
                          | wcardnum   > maxWcards   = error $ printf "Too many wildcards, limit is %d" maxWcards
                          | otherwise = getScores =<< parMap rdeepseq genPlaysAt (range tableBounds) where

    table = parseTable tbl trie 
    (wcardnum, rack) = first length $ partition (==wildcard) rck

    maybeDel :: String -> TrieNode -> Maybe (TrieNode, String)
    maybeDel []     n = Nothing
    maybeDel (x:xs) n | x == val n = Just (n, xs)
                      | otherwise  = second (x:) `fmap` maybeDel xs n

    leftParts :: [PrefixData]
    leftParts = (trie, "", (wcardnum, rack)): next trie rack wcardnum where

        next n r w = concat $ no_wcard ++ with_wcard where
            chs        = getChildren n 
            no_wcard   = [go id ch r' w | not (null r), Just (ch, r') <- map (maybeDel r) chs]
            with_wcard = [go toLower ch r (w - 1) | w /= 0, ch <- chs]

        go f n r w = (n, [c], (w, r)): [(n, c:p, wr) | (n, p, wr) <- next n r w] where
            c = f (val n)

    leftPartMemo :: Array Int [PrefixData]
    leftPartMemo = accumArray (flip (:)) [] (0, maxRackSize) [(length pref, a) | a@(_, pref, _) <- leftParts]

    getLeftParts :: Int -> [PrefixData]
    getLeftParts l = [0..l] >>= (leftPartMemo!)

    getRightParts :: CellIndex -> PrefixData -> [(String, String)]
    getRightParts i (n, pref, (w, r)) = map (pref,) $ concat $ next n r w i (table ! i) where

        next n r w (i,j) (Filled c) = [go (const c) ch r w (i, j + 1) | ch <- getChildren n, val ch == toUpper c]
        next n r w (i,j) cell       = no_wcard ++ with_wcard where
            i'         = (i, j + 1)
            chs        = (if isEmpty cell then id else filter (hasLetter (lset cell) . val)) (getChildren n)
            no_wcard   = [go id ch r' w i' | not (null r), Just (ch, r') <- map (maybeDel r) chs]
            with_wcard = [go toLower ch r (w - 1) i' | w /= 0, ch <- chs]

        go f n r w i = word ++ rest where
            c    = f (val n)
            cell = table ! i
            inb  = inBounds i
            word = [[c] | eow n, (not inb) || (not $ isFilled cell)]
            rest = [c:x | inb, x <- concat $ next n r w i cell]


    genPlaysAt :: CellIndex -> (CellIndex, [(String, String)])
    genPlaysAt i | not $ isAnchor $ table ! i = (i, [])
                 | not (null leftWord)        = (i, getRightParts i (leftNode, leftWord, (wcardnum, rack)))
                 | otherwise                  = (i, getLeftParts prefLen >>= getRightParts i) where 

        goLeft = map (table!) $ drop 1 $ stepLeft i
        leftWord = reverse $ map (\(Filled c) -> c) $ takeWhile isFilled goLeft
        leftNode = getNodeOfWord trie leftWord
        prefLen  = min maxRackSize (length $ takeWhile isEmpty $ goLeft)

    getScores :: (CellIndex, [(String, String)]) -> [Play]
    getScores ((i, j), playwords) = map go playwords where

        go (a, b) = Play dir start' totalScore word where
            start   = (i, j - (fromIntegral $ length a))
            cells   = map (table!) (stepRight start)
            bonuses = map (bonusTable!) (stepRight start)
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

                wmods' = (if elem bonus [WS2, WS3] && notfill then (bonus:) else id) wmods
                in (wsc + lscore, csc', wmods', bingo + fromEnum notfill)

            (wsc, csc, wmods, bingo) = foldl' process (0, 0, [], 0) (zip3 word cells bonuses)
            totalScore = csc + foldl' wordMod wsc wmods + 50 * fromEnum (bingo == 7)


genAllPlays :: [String] -> TrieNode -> String -> [Play]
genAllPlays table trie rack = sortBy (flip $ comparing score) plays where
    plays = zip [H, V] [table, transpose table] >>= \(d, t) -> genPlays d t trie rack


showPlay :: [String] -> Play -> IO ()
showPlay table p@(Play d l s w) = let
    as = zip ((if d == H then stepRight else stepDown) l) w
    bs = zip (range tableBounds) (concat table)

    insert ((i, a):as) ((j, b):bs)
        | i == j    = a: insert as bs
        | otherwise = b: insert ((i, a):as) bs
    insert a b = map snd a ++ map snd b

    in do putStrLn (show p)
          mapM_ print $ chunksOf (length table) (insert as bs)


main = do
    trie <- readTrie
    let solutions = genAllPlays table trie "ETAOI"  
    printf "Number of solutions: %d\n" (length solutions)
    mapM_ (showPlay table) (take 5 solutions)


table =  [
    "          CARAT",
    "         BA    ",
    "          I    ",
    "          R    ",
    "   REHARDEN    ",
    "         N     ",
    "         T     ",
    "      TOUR    E",
    "         YONDER",
    "              O",
    "              T",
    "              I",
    "              C",
    "               ",
    "               "]