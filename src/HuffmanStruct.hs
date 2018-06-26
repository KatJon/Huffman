module HuffmanStruct where

import Data.Binary
import Data.Binary.Put
import Data.Binary.Get

newtype Freq a = Freq (a, Int) 
    deriving (Eq, Show)

instance Eq a => Ord (Freq a) where
    compare (Freq (_, p)) (Freq (_, q)) = compare p q

newtype Code a s = Code (a, Int, s)
    deriving (Eq, Show)

data HuffmanTree a = Leaf Int a | Node Int (HuffmanTree a) (HuffmanTree a)
    deriving (Eq, Show)

instance Eq a => Ord (HuffmanTree a) where
    compare l r = compare (value l) (value r)
    
value :: HuffmanTree a -> Int
value (Leaf x _) = x
value (Node x _ _) = x

fromFreq :: Freq a -> HuffmanTree a
fromFreq (Freq (x,i)) = Leaf i x

encodeTree :: HuffmanTree Word8 -> Put
encodeTree (Leaf _ c) = do 
    putWord8 0
    putWord8 c
encodeTree (Node _ l r) = do
    putWord8 1
    encodeTree l
    encodeTree r

decodeTree :: Get (HuffmanTree Word8)
decodeTree = do
    fst <- getWord8
    case fst of
        0 -> do
            c <- getWord8
            return $ Leaf 0 c
        1 -> do
            left <- decodeTree
            right <- decodeTree
            return $ Node 0 left right

data HuffmanHeader = HuffmanHeader {
    contentSize :: Word32,
    codeTree :: HuffmanTree Word8
} deriving (Show, Eq)

encodeHeader :: HuffmanHeader -> Put
encodeHeader hdr = do
    putWord32le $ contentSize hdr
    encodeTree $ codeTree hdr

decodeHeader :: Get HuffmanHeader
decodeHeader = do
    size <- getWord32le
    tree <- decodeTree
    return $ HuffmanHeader size tree