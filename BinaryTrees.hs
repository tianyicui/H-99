{-# LANGUAGE NoMonomorphismRestriction #-}

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

symmetric (Branch _ ta tb) = mirror ta tb
symmetric _ = False

mirror Empty Empty = True
mirror (Branch _ t1a t1b)
       (Branch _ t2a t2b) =
    (mirror t1a t2b) && (mirror t1b t2a)
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
