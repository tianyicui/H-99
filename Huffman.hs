module Huffman (huffman) where

import Data.List
import Data.Ord (comparing)

data Tree a b
    = Leaf b a
    | Node b (Tree a b) (Tree a b)

huffman xs =
    sortBy (comparing fst) $ huffman' $ sortBy cmp [ Leaf i c | (c,i) <- xs ]

huffman' [x] = treeToResult x
huffman' (x:y:s) = huffman' $ insertBy cmp (merge x y) s

freq (Leaf i _  ) = i
freq (Node i _ _) = i

cmp = comparing freq

merge x y = Node (freq x + freq y) x y

treeToResult (Leaf _ c)   = [(c, "")]
treeToResult (Node _ x y) =
    (add '0' $ treeToResult x) ++ (add '1' $ treeToResult y)
        where add p r = [ (c, p:s) | (c, s) <- r ]
