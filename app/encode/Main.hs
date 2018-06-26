module Main where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BCL (putStrLn)

import Data.Word
import Data.Binary.Put
import Data.Binary.Get
import Data.Ratio
import Text.Printf
import System.Environment

import Huffman
import HuffmanStruct

main :: IO ()
main = do
    args <- getArgs
    let filename = head args
    contents <- BS.readFile filename
    let size = fromIntegral $ 8 * (BS.length contents)
    let hist = getHistogram contents
    let huffmanTree = huffman hist
    let list = codeList huffmanTree
    let header = HuffmanHeader size huffmanTree
    printInfo header list
    print header
    let encodedHeader = runPut $ encodeHeader header
    printBinary encodedHeader
    return ()
    

printInfo :: HuffmanHeader -> [Code Word8 String] -> IO ()
printInfo hdr list = do
    let size = fromIntegral $ contentSize hdr
    let tree = codeTree hdr
    let encodedHeader = runPut $ encodeHeader hdr
    let headerSize = 8 * fromIntegral (BL.length encodedHeader)
    let outputSize = huffmanSize list + headerSize
    putStrLn $ "Size of input: " ++ show size
    putStrLn $ "Header size: " ++ show headerSize
    putStrLn $ "Size of output: " ++ show outputSize
    let compressionRatio = frac outputSize size
    printf "Ratio: %.2f%%\n" (100 * compressionRatio)

frac :: (Num a, Real a) => a -> a -> Double
frac a b = a' / b'
    where   rat = fromRational . toRational
            a' = rat a
            b' = rat b

printBinary :: BL.ByteString -> IO ()
printBinary bs
    | BL.null bs = putStrLn ""
    | otherwise = do
            let h = BL.head bs
            let t = BL.tail bs
            printf "%b" h
            printBinary t