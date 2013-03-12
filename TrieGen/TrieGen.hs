import Data.Binary
import Data.Bits
import Data.Int
import Data.Array.Unboxed
import qualified Data.ByteString as B
import Data.List.Split
import System.Environment

main = do
    inputFile : outputFile : _ <- getArgs
    trie <- B.readFile inputFile

    let out :: UArray Int32 Int32
        out = listArray (0, fromIntegral $ div (B.length trie) 4 - 1) [
                sum (zipWith shiftL (map fromIntegral nums) [0, 8..]) | 
                nums <- chunksOf 4 $ B.unpack trie]
        
    encodeFile outputFile out