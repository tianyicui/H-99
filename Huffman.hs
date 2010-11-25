module Huffman (huffman) where

import Data.List
import Data.Ord

data Tree = Leaf Int Char
          | Node Int Tree Tree

huffman :: [(Char, Int)] -> [(Char, String)]
huffman xs =
    sortBy (comparing fst) $ huffman' [ Leaf i c | (c,i) <- xs ]

huffman' :: [Tree] -> [(Char, String)]
huffman' [x] = treeToResult x
huffman' xs  =
    let (x:y:s) = sortBy (comparing freq) xs -- TODO: improve the time complexity here
    in huffman' $ (merge x y) : s

freq (Leaf i _  ) = i
freq (Node i _ _) = i

merge x y = Node (freq x + freq y) x y

treeToResult (Leaf _ c)   = [(c, "")]
treeToResult (Node _ x y) =
    (add '0' $ treeToResult x) ++ (add '1' $ treeToResult y)
        where add p r = [ (c, p:s) | (c, s) <- r ]
