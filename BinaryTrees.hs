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

add x Empty = Branch x Empty Empty
add x t@(Branch y ta tb)
    | x < y = Branch y (add x ta) tb
    | x > y = Branch y ta (add x tb)
    | otherwise = t

testSymmetric = symmetric . construct

symCbalTrees = (filter symmetric) . cbalTree

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
