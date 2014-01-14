{-# LANGUAGE 
      RecordWildCards
    , BangPatterns
    , LambdaCase
    , PatternGuards
    , ScopedTypeVariables 
    , TupleSections #-}

import qualified Data.Array.Unboxed as A
import qualified Data.DAWG.Packed as D

import Control.Arrow
import Control.DeepSeq
import Control.Parallel.Strategies

import Data.Char
import Data.Function
import Data.Ix
import Data.List
import Data.Maybe
import Data.Tuple


type Rack      = String
type WildCards = Int
type Score     = Int
type Move      = (Score, [((Int, Int), Char)])

data Cell = Empty 
          | Filled {
            {- UNPACK -} _char :: !Char}
          | Anchor {
            {- UNPACK -} _crossScore :: !Score,
                         _constraint :: !Rack} 
           deriving (Eq, Show)


isFilled :: Cell -> Bool 
isFilled = \case Filled{} -> True; _ -> False


instance NFData Cell where
    rnf Empty               = ()
    rnf (Filled c)          = rnf c
    rnf (Anchor csc constr) = rnf csc `seq` rnf constr `seq` ()

data Bonus = Nil | LS2 | LS3 | WS2 | WS3 deriving (Enum, Eq, Show)

data GameData = GameData {
    _gaddag   :: !D.Node,
    _bonuses  :: [[Bonus]],
    _charScore :: Char -> Score}


defaultScores :: A.UArray Int Score
defaultScores = A.array (ord 'A', ord 'Z') $ map (first ord) $
    [('E', 1), ('A', 1),  ('I', 1), ('O', 1), ('N', 1), 
    ('R', 1), ('T', 1), ('L', 1), ('S', 1), ('U', 1), 
    ('D', 2), ('G', 2), ('B', 3), ('C', 3), ('M', 3), 
    ('P', 3), ('F', 4), ('H', 4), ('V', 4), ('W', 4), 
    ('Y', 4), ('K', 5), ('J', 8), ('X', 8), ('Q', 10), ('Z', 10)]


charScore :: Char -> Int
charScore c | isUpper c = defaultScores A.! (ord c)
            | otherwise = 0


scrabbleBonuses :: [[Bonus]]
scrabbleBonuses = (map . map) toEnum
    [[4, 0, 0, 1, 0, 0, 0, 4, 0, 0, 0, 1, 0, 0, 4],
     [0, 3, 0, 0, 0, 2, 0, 0, 0, 2, 0, 0, 0, 3, 0],
     [0, 0, 3, 0, 0, 0, 1, 0, 1, 0, 0, 0, 3, 0, 0],
     [1, 0, 0, 3, 0, 0, 0, 1, 0, 0, 0, 3, 0, 0, 1],
     [0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0],
     [0, 2, 0, 0, 0, 2, 0, 0, 0, 2, 0, 0, 0, 2, 0],
     [0, 0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0],
     [4, 0, 0, 1, 0, 0, 0, 3, 0, 0, 0, 1, 0, 0, 4],
     [0, 0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0],
     [0, 2, 0, 0, 0, 2, 0, 0, 0, 2, 0, 0, 0, 2, 0],
     [0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0],
     [1, 0, 0, 3, 0, 0, 0, 1, 0, 0, 0, 3, 0, 0, 1],
     [0, 0, 3, 0, 0, 0, 1, 0, 1, 0, 0, 0, 3, 0, 0],
     [0, 3, 0, 0, 0, 2, 0, 0, 0, 2, 0, 0, 0, 3, 0],
     [4, 0, 0, 1, 0, 0, 0, 4, 0, 0, 0, 1, 0, 0, 4]]


parseRow :: GameData -> String -> [Cell]
parseRow GameData{..} row = let
    go ls (' ':rs) = let
        left    = reverse $ takeWhile (/=' ') ls
        right   = takeWhile (/=' ') rs
        context = left ++ right 
        score   = sum $ map _charScore context

        constraint = [ c | c <- ['A'..'Z'], 
            D.memberBy ((==) `on` toUpper) (left ++ c : right) _gaddag]

        cell | null context = Empty
             | otherwise    = Anchor score constraint

        in cell : go (' ':ls) rs 

    go ls (r:rs) = Filled r : go (r:ls) rs
    go _  []     = []

    in go [] row


parseTable :: GameData -> [String] -> ([[Cell]], [[Cell]])
parseTable gdat@GameData{..} table = let
    assertSize x 
        | all (==15) $ length table : map length table = x
        | otherwise = error "Invalid table size"

    merge  Anchor{} b@Anchor{} = b
    merge  Anchor{} _          = Anchor 0 ['A'..'Z']
    merge _             b      = b

    merge' = (zipWith . zipWith) merge
    [horizontal, vertical] = (map . map) (parseRow gdat) [table, transpose table]

    in assertSize
        (merge'  horizontal (transpose vertical), 
         merge'  vertical   (transpose horizontal))


mkChoices :: D.Node -> Cell -> Rack -> WildCards -> [(D.Node, Rack, WildCards)]
mkChoices node cell rack wcards = let

    constrainNodes (n:ns) (c:cs) = case compare (D.char n) c of
        LT -> constrainNodes ns (c:cs)
        GT -> constrainNodes (n:ns) cs
        EQ -> n : constrainNodes ns cs 
    constrainNodes _ _ = []

    chd | Anchor _ cs <- cell = constrainNodes (D.children node) cs
        | otherwise = D.children node 

    choose as (b:bs) (n:ns) = case compare b (D.char n) of
        LT -> choose (b:as) bs (n:ns)
        GT -> choose as (b:bs) ns
        EQ -> (n, reverse as ++ bs, wcards) : choose (b:as) bs ns
    choose _ _ _ = []

    wcardUse = [(n {D.char = toLower $ D.char n}, rack, wcards - 1) | 
                    wcards > 0, n <- chd, D.char n /= '*']

    in wcardUse ++ choose [] rack chd 


leftSearch :: D.Node -> Rack -> WildCards -> [Cell] -> [String]
leftSearch node rack wcards cells = let
    go node rack wcards (Empty:cells) = let
        choices  = mkChoices node Empty rack wcards
        newWords = [[D.char n'] | (n', _, _) <- choices, D.endOfWord n']
        rest = [D.char n': pref |
            (n', rack', wcards') <- choices,
            pref <- go n' rack' wcards' cells]
        in newWords ++ rest
    go _ _ _ _ = []

    in maybe [] 
        (\node -> go node rack wcards cells)
        (let check | n:ns <- D.children node,
                     D.char n == '*' = Just n
                   | otherwise = Nothing
         in check)


contextSearch :: GameData -> Rack -> WildCards -> [Cell] -> [Cell] -> [(String, String)]
contextSearch GameData{..} rack wcards lcells rcells = let

    rightSearch :: D.Node -> Rack -> WildCards -> [Cell] -> [(String, String)]
    rightSearch _ _ _ [] = []
    rightSearch node rack wcards (cell:cells) = 

        case span isFilled (cell:cells) of

            ([], _) -> let
                choices = mkChoices node cell rack wcards
                (newWords, prefixes) 
                    | Filled{}:_ <- cells = ([], [])
                    | otherwise = (
                        [([D.char n'], "") | 
                            (n', _, _) <- choices, D.endOfWord n'],
                        [([D.char n'], pref) | 
                            (n', rack', wcards')  <- choices,
                            pref <- leftSearch n' rack' wcards' lcells])

                rest = [ (D.char n': suff, pref) |
                    (n', rack', wcards') <- choices,
                    (suff, pref) <- rightSearch n' rack' wcards' cells]

                in prefixes ++ newWords ++ rest

            (filledCells, restOfCells) -> let
                word = map _char filledCells
                in maybe [] 
                    (\node -> let
                        newWord  = [(word, "") | D.endOfWord node]
                        prefixes = map (word,) $ leftSearch node rack wcards lcells
                        rest     = map (first (word++)) $ rightSearch node rack wcards restOfCells
                        in prefixes ++ newWord ++ rest)
                    (D.lookupPrefixBy ((==) `on` toUpper) word node)


    results :: [(String, String)]
    results = case takeWhile isFilled lcells of
        []    -> rightSearch _gaddag rack wcards rcells
        cells -> let
            leftWord = map _char (reverse cells)
            in maybe []
                (\startNode -> 
                    map (second (reverse leftWord++)) $ rightSearch startNode rack wcards rcells)
                (D.lookupPrefixBy ((==) `on` toUpper) leftWord _gaddag)


    in case rcells of
        Anchor{}:_ -> results
        _ -> []


computeScore :: GameData -> [(Bonus, Cell, Char)] -> Score
computeScore GameData{..} word = let

    letterMod = \case
        LS2 -> (*2)
        LS3 -> (*3)
        _   -> id 

    wordMod = \case
        WS2 -> (*2)
        WS3 -> (*3)
        _   -> id 

    go (wordScore, wordMul, crossScore, lettersUsed) (bonus, cell, c) = let
        letterScore = _charScore c
        isFilled' = isFilled cell

        crossScore' | Anchor score _ <- cell, score /= 0 = 
                        crossScore + wordMod bonus (score + letterMod bonus letterScore)
                    | otherwise = crossScore

        lettersUsed' = lettersUsed + fromEnum (not isFilled')

        (wordMul', wordScore') | isFilled' = (wordMul, wordScore + letterScore)
                               | otherwise = (wordMod bonus wordMul,
                                              wordScore + letterMod bonus letterScore)

        in (wordScore', wordMul', crossScore', lettersUsed') 

    (wordScore, wordMul, crossScore, lettersUsed) = foldl' go (0, 1, 0, 0) word
    bingo = 50 * fromEnum (lettersUsed == 7) 

    in wordMul * wordScore + crossScore + bingo 


tableSearch :: GameData -> Rack -> WildCards -> [[Cell]] -> [Move]
tableSearch gdat@GameData{..} rack wcards table = let

    rowSearch :: Int -> [Bonus] -> [Cell] -> [Move]
    rowSearch i bonuses cells = let
        go j lbs (rb:rbs) lcells (rc:rcells) = let
            moves = [(score, reverse left ++ right) |
                (suff, pref) <- contextSearch gdat rack wcards lcells (rc:rcells),
                let 
                    pref' = zip3 lbs lcells pref
                    suff' = zip3 (rb:rbs) (rc:rcells) suff 
                    score = computeScore gdat (pref' ++ suff')
                    left  = zip (map (i,) [j - 1, j - 2 ..]) pref
                    right = zip (map (i,) [j ..]) suff ] 
            in moves ++ go (j + 1) (rb:lbs) rbs (rc:lcells) rcells 
        go _ _ _ _ _ = []

        in go 0 [] bonuses [] cells 

    in concat (zipWith3 rowSearch [0..] _bonuses table `using` parList rdeepseq)


genMoves :: GameData -> Rack -> [String] -> [Move]
genMoves gdat rack table = let

    (wcards, rack')  = (length *** sort) $ partition (=='_') rack 
    (hparse, vparse) = parseTable gdat table
    horizontal       = tableSearch gdat rack' wcards hparse 

    vertical = [(score, map (first swap) move) |
        (score, move) <- tableSearch gdat rack' wcards vparse] 

    in horizontal ++ vertical


countingSortBy :: forall a i. Ix i => (a -> i) -> [a] -> [a]
countingSortBy _ [] = []
countingSortBy f xs = let

    minmax (x:xs) = foldl' go (x, x) xs where
        go (!mn, !mx) x = (min mn x, max mx x) 
        
    is = map f xs

    acc :: A.Array i [a]
    acc = A.accumArray (flip (:)) [] (minmax is) (zip is xs)

    in concat $ A.elems acc


main = do
    gaddag <- D.fromFile "twl06.gaddag"

    let defConf = GameData gaddag scrabbleBonuses charScore
        sols = countingSortBy (negate . fst) $ genMoves defConf refRack refTable

    mapM_ print $ take 10 sols
    print $ length sols




-- Test Reference data:

refRack = "ETAOI__"

refTable =  [
    "          CARAT",
    "          A    ",
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

-- number of solutions: 57953