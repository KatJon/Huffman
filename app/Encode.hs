module Main where

import Huffman

freq = [
        HWord ('a', 70), 
        HWord ('b', 3), 
        HWord ('c', 20), 
        HWord ('d', 37)
    ]

main :: IO ()
main = do
    print $ huffman freq
