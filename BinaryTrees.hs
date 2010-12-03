{-# LANGUAGE NoMonomorphismRestriction #-}

import Data.List
import Data.Maybe

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

isTree x = case x of
    Empty        -> True
    Branch _ _ _ -> True

cbalTree 0 = [Empty]
cbalTree n =
    [ Branch 'x' ta tb
    | let p0 = (n-1) `div` 2
    , let p1 =  n    `div` 2
    , p <- [p0..p1]
    , let q = n - 1 - p
    , ta <- cbalTree p
    , tb <- cbalTree q
    ]

symmetric Empty          = True
symmetric (Branch _ l r) = mirror l r

mirror Empty Empty = True
mirror (Branch _ l1 r1) (Branch _ l2 r2) =
    mirror l1 r2 && mirror l2 r1
mirror _ _ = False

construct = foldr add Empty . reverse
construct' = foldl' (flip add) Empty

add x Empty = Branch x Empty Empty
add x t@(Branch y ta tb)
    | x < y = Branch y (add x ta) tb
    | x > y = Branch y ta (add x tb)
    | otherwise = t

testSymmetric = symmetric . construct

symCbalTrees = filter symmetric . cbalTree

hbalTree 0 = [Empty]
hbalTree 1 = [Branch 'x' Empty Empty]
hbalTree n =
    [ Branch 'x' ta tb
    | (p, q) <- [(n-2,n-1), (n-1,n-1), (n-1,n-2)]
    , ta <- hbalTree p
    , tb <- hbalTree q
    ]

-- a correct but too slow version
hbalTreeNodes' 0 = [Empty]
hbalTreeNodes' n =
    [ Branch 'x' ta tb 
    | p <- [0..(n-1)]
    , let q = n - 1 - p
    , ta <- hbalTreeNodes' p
    , tb <- hbalTreeNodes' q
    , abs ((height ta) - (height tb)) <= 1
    ]

height Empty = 0
height (Branch _ x y) = 1 + max (height x) (height y)

----

minNodes h = seq !! h
    where seq = 0 : 1 : zipWith (\x y -> 1+x+y) seq (tail seq)
maxNodes h = 2^h - 1 :: Int
minHeight n = ceiling $ logBase 2 $ fromIntegral (n+1)
maxHeight n = (fromJust $ find (\h -> minNodes h > n) [1..]) - 1

hbalTreeNodes n =
    concat [ calc n h | h <- [ minHeight n .. maxHeight n ] ]
  where
   calc _ 0 = [Empty]
   calc _ 1 = [Branch 'x' Empty Empty]
   calc n h =
      [ Branch 'x' tl tr
      | (hl, hr) <- [(h-1,h-2), (h-1,h-1), (h-2,h-1)]
      , let min_nl = max (minNodes hl) (n - 1 - maxNodes hr)
      , let max_nl = min (maxNodes hl) (n - 1 - minNodes hr)
      , nl <- [min_nl .. max_nl]
      , let nr = n - 1 - nl
      , tl <- calc nl hl
      , tr <- calc nr hr
      ]

countLeaves Empty                  = 0
countLeaves (Branch _ Empty Empty) = 1
countLeaves (Branch _ l     r    ) = countLeaves l + countLeaves r

leaves Empty = []
leaves (Branch x Empty Empty) = [x]
leaves (Branch _ left  right) = leaves left ++ leaves right

internals Empty = []
internals (Branch _ Empty Empty) = []
internals (Branch x left  right) = x : (internals left ++ internals right)

atLevel Empty                 _ = []
atLevel (Branch a left right) 1 = [a]
atLevel (Branch _ left right) n = left `atLevel` (n-1) ++ right `atLevel` (n-1)

-- think as make a binary heap using an array
completeBinaryTree n = go 1 where
    go m = if m > n
           then Empty
           else Branch 'x' (go $ m*2) (go $ m*2+1)

treeNodes Empty = 0
treeNodes (Branch _ left right) = 1 + treeNodes left + treeNodes right

treeEqual Empty Empty = True
treeEqual (Branch _ l1 r1) (Branch _ l2 r2) =
    (treeEqual l1 l2) && (treeEqual r1 r2)
treeEqual _ _ = False

isCompleteBinaryTree t = treeEqual t $ completeBinaryTree $ treeNodes t

instance Functor Tree where
    fmap f Empty = Empty
    fmap f (Branch x l r) = Branch (f x) (fmap f l) (fmap f r)

inorder t = go 1 t where
    go _ Empty = Empty
    go n (Branch _ l r) =
        let ln = treeNodes l
        in Branch (n+ln) (go n l) (go (n+ln+1) r)

depth t = go 1 t where
    go _ Empty = Empty
    go n (Branch _ l r) =
        Branch n (go (n+1) l) (go (n+1) r)

treeZipWith _ Empty _ = Empty
treeZipWith _ _ Empty = Empty
treeZipWith f (Branch x1 l1 r1) (Branch x2 l2 r2) =
    Branch (f x1 x2) (treeZipWith f l1 l2) (treeZipWith f r1 r2)

treeZipWith3 f t1 t2 t3 =
    treeZipWith ($) (treeZipWith f t1 t2) t3

layout t = treeZipWith3 (\x y z -> (x,(y,z))) t (inorder t) (depth t)
