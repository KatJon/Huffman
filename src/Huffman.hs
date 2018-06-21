module Huffman where

import Data.PQueue.Min
data HuffmanTree a = Leaf Int a | Node Int (HuffmanTree a) (HuffmanTree a)
    deriving (Eq, Show)

value :: HuffmanTree a -> Int
value (Leaf x _) = x
value (Node x _ _) = x

fromHWord :: HWord a -> HuffmanTree a
fromHWord (HWord (x,i)) = Leaf i x

instance Eq a => Ord (HuffmanTree a) where
    compare l r = compare (value l) (value r)

newtype HWord a = HWord (a, Int) 
    deriving (Eq, Show)

instance Eq a => Ord (HWord a) where
    compare (HWord (_, p)) (HWord (_, q)) = compare p q


huffman :: Ord a => [HWord a] -> HuffmanTree a
huffman list = runHuffman queue
    where   queue = fromList $ Prelude.map fromHWord list
            
runHuffman :: Eq a => MinQueue (HuffmanTree a) -> HuffmanTree a
runHuffman q = case step of
        Just q' -> runHuffman q'
        Nothing -> findMin q
    where   step = do
                (e1,q1) <- minView q
                (e2,q2) <- minView q1
                let e' = Node (value e1 + value e2) e1 e2
                return $ insert e' q2
