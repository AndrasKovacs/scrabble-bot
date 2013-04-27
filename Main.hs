
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
    dawg <- D.fromFile dawgPath
    return $ genAllPlays table dawg rack

main = do
    solutions <- getSolutions
    printf "Number of solutions: %d\n\n" (length solutions)
    mapM_ (showPlay table) (take numSolutionsShown solutions)


-- Modify the following:

numSolutionsShown = 20

rack = "RECAO__"

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