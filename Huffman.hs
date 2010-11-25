module Huffman (huffman) where

data Tree = Leaf Int Char
          | Node Int Tree Tree

huffman :: [(Char, Int)] -> [(Char, String)]
huffman xs =
    huffman' [ Leaf i c | (c,i) <- xs ]

huffman' = undefined
