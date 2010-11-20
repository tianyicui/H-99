data EncodedList a = Single a | Multiple Int a deriving Show

decodeModified = concatMap func where
    func (Single x) = [x]
    func (Multiple n x) = replicate n x
