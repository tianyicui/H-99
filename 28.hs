import Data.List (sortBy)

lsort = sortBy (\x y -> compare (length x) (length y))
