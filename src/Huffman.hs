module Huffman where

import Data.Word
import Data.Maybe
import Data.Binary.Put
import Data.Binary.Get

import qualified Data.PQueue.Min as PQ
import qualified Data.Map.Strict as Map
import qualified Data.ByteString as BS

import HuffmanStruct

type Map = Map.Map
type ByteString = BS.ByteString
type MinQueue = PQ.MinQueue

huffman :: Ord a => [Freq a] -> HuffmanTree a
huffman list = runHuffman queue
    where   queue = PQ.fromList $ Prelude.map fromFreq list
            
runHuffman :: Eq a => MinQueue (HuffmanTree a) -> HuffmanTree a
runHuffman q = case step of
        Just q' -> runHuffman q'
        Nothing -> PQ.findMin q
    where   step = do
                (e1,q1) <- PQ.minView q
                (e2,q2) <- PQ.minView q1
                let e' = Node (value e1 + value e2) e1 e2
                return $ PQ.insert e' q2

getHistogram :: ByteString -> [Freq Word8]
getHistogram bs = map Freq $ Map.toList hist 
    where   hist = makeHistogram bs

makeHistogram :: ByteString -> Map Word8 Int
makeHistogram bs = BS.foldl count Map.empty bs
    where   count map c = Map.alter update c map
            update x = Just $ 1 + fromMaybe 0 x

codeList :: HuffmanTree a -> [Code a String]
codeList t = run t [] []
    where
        run (Leaf v c) res acc = (Code (c, v, reverse acc)) : res
        run (Node _ l r) res acc = 
            let left = run l res ('0':acc) in
                run r left ('1':acc)

huffmanSize :: [Code a String] -> Int
huffmanSize list = foldl addCode 0 list
    where addCode acc (Code (_, v, s)) = acc + v * (length s)
