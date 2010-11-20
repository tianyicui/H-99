slice xs i j = take (j-i+1) (drop (i-1) xs)

slice'' xs i j = map snd
               $ filter (\(x,_) -> x >= i && x <= j)
               $ zip [1..] xs

---- so I can also write like this
---- XXX: works with ghc 6.12.3 but not 7.0.1!
-- slice' xs (i+1) j = take (j-i) $ drop i xs
