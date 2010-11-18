data EncodedList a = Single a | Multiple Int a deriving Show

encodeModified [] = []
encodeModified [a] = [Single a]
encodeModified (x:xs) = doEncode x (encodeModified xs) where
    doEncode x (Single y : xs) =
        if x == y
        then (Multiple 2 y):xs
        else (Single x):(Single y):xs
    doEncode x (Multiple n y : xs) =
        if x == y
        then (Multiple (1+n) y):xs
        else (Single x):(Multiple n y):xs
