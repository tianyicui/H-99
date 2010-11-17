-- the simple pattern matching way is omitted

encode :: (Eq a) => [a] -> [(Integer, a)]
encode = foldr func []
    where
    func x ((n,y):sx)
        | x == y    = ((n+1, y):sx)
    func x xs       = ((1,x):xs)

---- TODO: read http://www.haskell.org/haskellwiki/99_questions/Solutions/10
