{-# LANGUAGE BangPatterns, PatternGuards, LambdaCase, RecordWildCards #-}

import qualified Data.IntMap as M
import qualified Data.Array.Unboxed as A
import qualified Data.Binary as B
import Data.List hiding (insert)
import Control.Monad.State
import Data.Int
import Data.Char
import Data.Bits
import System.Exit
import Data.Function
import System.Directory
import System.Environment
import System.TimeIt

data Trie = Node {
    {- UNPACK -} eow  :: !Bool,     
    {- UNPACK -} char :: !Char,    
    {- UNPACK -} chs  :: ![Trie]}

pack :: Int -> Char -> Bool -> Bool -> Int32
pack chi char eol eow = 
    fromIntegral $ foldl' (.|.) 0 [
        chi `shiftL` 10, 
        ord char `shiftL` 2, 
        fromEnum eol `shiftL` 1, 
        fromEnum eow]

setEol :: Int32 -> Int32
setEol = (.|. 2)

insert :: String -> Trie -> Trie 
insert []     !n = n {eow = True}
insert (x:xs) !n@(Node {chs = chs})
    | c:cs <- chs,
      char c == x = n {chs = insert xs c:cs}
    | otherwise   = n {chs = (insert xs (Node False x [])):chs}

reduce :: Trie -> State (M.IntMap Int, [[Int32]], Int) (Int, Int32)
reduce !node@(Node{..}) = do
    (hashes, chs) <- unzip `fmap` mapM reduce chs
    (memo, l, i)  <- get

    let getHash   = foldl' (\s x -> 3074457345618258791 * (s + ord x))
        childHash = getHash 0 (show =<< hashes)
        nodeHash  = getHash childHash (char:show eow)
        chs' | c:rest <- chs = setEol c: rest 
             | otherwise     = chs 
    
    case M.lookup childHash memo of
        Nothing -> do
            put (M.insert childHash (i + 1) memo, chs':l, i + length chs')
            return (nodeHash, pack (i + 1) char False eow)
        Just i' -> do
            return (nodeHash, pack i' char False eow)

toArray :: Trie -> A.UArray Int32 Int32
toArray n = let
    ((h, root), (_, l, _)) = runState (reduce n) (M.singleton 0 0, [[0]], 0)
    l' = reverse $ (setEol root): concat l
    in A.listArray (0, fromIntegral $ length l' - 1) l'

main = timeIt $ do 
    inp : out : [] <- do
        args <- getArgs
        if length args == 2 
            then do 
                check <- doesFileExist $ head args
                if check 
                    then return args
                    else do 
                        putStrLn "Invalid input file"
                        exitFailure
            else do 
                putStrLn "Usage: dawg-gen.exe [inputFile outPutfile]"
                exitFailure

    ws <- lines `fmap` readFile inp 
    
    if and (zipWith (\a b -> (all isUpper a) && (a <= b)) ws (tail ws)) 
        then do 
            let trie = foldl' (flip insert) (Node False '\0' []) ws
            B.encodeFile out (toArray trie)
        else 
            putStrLn "The dictionary should be in upper case and alphabetically sorted."