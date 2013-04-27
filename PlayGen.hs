
{-# LANGUAGE LambdaCase, TupleSections, RecordWildCards, BangPatterns #-}

module PlayGen (genAllPlays, showPlay) where

import qualified Data.DAWG as D
import Data.Int                     
import Data.Array.Unboxed                                            
import Data.List                    
import Data.List.Split           
import Data.Char                                       
import Control.Arrow               
import Control.Parallel.Strategies
import Data.Ord                    
import Data.Tuple                  
import Text.Printf            
import GameData

maxRackSize = 7
maxWcards = 2

genPlays :: Direction -> [String] -> D.Node -> String -> [Play]
genPlays dir tbl dawg rck | length rck > maxRackSize = error $ printf "Rack too big, limit is %d" maxRackSize
                          | wcardnum   > maxWcards   = error $ printf "Too many wildcards, limit is %d" maxWcards
                          | otherwise = concat $ parMap rdeepseq (getScores . genPlaysAt) (range tableBounds) where


    table = parseTable tbl dawg 
    (wcardnum, rack) = first length $ partition (==wildcard) rck 

    maybeDel :: String -> D.Node -> Maybe (D.Node, String)
    maybeDel []     n = Nothing
    maybeDel (x:xs) n | x == D.value n = Just (n, xs)
                      | otherwise  = second (x:) `fmap` maybeDel xs n

    leftParts :: [PrefixData]
    leftParts = (dawg, "", (wcardnum, rack)): next dawg rack wcardnum where

        next n r w = concat $ no_wcard ++ with_wcard where
            chs        = D.getChildren n 
            no_wcard   = [go id ch r' w | not (null r), Just (ch, r') <- map (maybeDel r) chs]
            with_wcard = [go toLower ch r (w - 1) | w /= 0, ch <- chs]

        go f n r w = (n, [c], (w, r)): [(n, c:p, wr) | (n, p, wr) <- next n r w] where
            c = f (D.value n)


    leftPartMemo :: Array Int [PrefixData]
    leftPartMemo = accumArray (flip (:)) [] (0, maxRackSize) [(length pref, a) | a@(_, pref, _) <- leftParts]


    getLeftParts :: Int -> [PrefixData]
    getLeftParts l = [0..l] >>= (leftPartMemo!)


    getRightParts :: CellIndex -> PrefixData -> [(String, String)]
    getRightParts i (n, pref, (w, r)) = map (pref,) $ concat $ next n r w i (table ! i) where

        next n r w (i,j) (Filled c) = [go (const c) ch r w (i, j + 1) | ch <- D.getChildren n, D.value ch == toUpper c]
        next n r w (i,j) cell       = no_wcard ++ with_wcard where
            i'         = (i, j + 1)
            chs        = (if isEmpty cell then id else filter (hasLetter (lset cell) . D.value)) (D.getChildren n)
            no_wcard   = [go id ch r' w i' | not (null r), Just (ch, r') <- map (maybeDel r) chs]
            with_wcard = [go toLower ch r (w - 1) i' | w /= 0, ch <- chs]

        go f n r w i = word ++ rest where
            c    = f (D.value n)
            cell = table ! i
            inb  = inBounds i
            word = [[c] | D.endOfWord n, (not inb) || (not $ isFilled cell)]
            rest = [c:x | inb, x <- concat $ next n r w i cell]


    genPlaysAt :: CellIndex -> (CellIndex, [(String, String)])
    genPlaysAt i = (i, plays) where

        plays | not $ isAnchor $ table ! i = []
              | not $ null leftWord        = getRightParts i (leftNode, leftWord, (wcardnum, rack))
              | otherwise                  = getLeftParts prefLen >>= getRightParts i

        goLeft   = map (table!) $ drop 1 $ stepLeft i
        leftWord = reverse $ map (\(Filled c) -> c) $ takeWhile isFilled goLeft
        leftNode = maybe (error "invalid word in scrabble table") id (D.lookupPrefix leftWord dawg)
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


genAllPlays :: [String] -> D.Node -> String -> [Play]
genAllPlays table dawg rack = sortBy (flip $ comparing score) plays where
    plays = zip [H, V] [table, transpose table] >>= \(d, t) -> genPlays d t dawg rack


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