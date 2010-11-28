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
