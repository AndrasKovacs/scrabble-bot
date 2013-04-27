{-# LANGUAGE LambdaCase #-}

import qualified Data.DAWG as D
import PlayGen
import GameData
import Paths_scrabble_bot

import System.Directory
import Text.Printf

dawgFile = "twl06.dawg"
dictFile = "dictionaries/TWL06.txt"

getSolutions :: IO [Play]
getSolutions = do
    dictPath <- getDataFileName dictFile
    dawgPath <- getDataFileName dawgFile
    dawg <- do
        doesFileExist dawgPath >>= \case
            True  -> D.fromFile dawgPath
            False -> do 
                ws <- fmap lines $ readFile dictPath
                let dawg = D.fromList ws
                D.toFile dawgPath dawg
                return dawg 
    return $ genAllPlays table dawg rack

main = do
    solutions <- getSolutions
    printf "Number of solutions: %d\n\n" (length solutions)
    mapM_ (showPlay table) (take numSolutionsShown solutions)


-- Modify the following:

numSolutionsShown = 20

rack = "RECA"

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