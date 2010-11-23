import Data.List (sortBy, groupBy)
import Data.Ord (comparing)

lsort = sortBy (\x y -> compare (length x) (length y))

----

-- this looks better
lsort' = sortBy (comparing length)

-- this solution is brilliant.
-- it's hard to write a O(NlogN) solution using only List processing.
lfsort lists = concat groups
    where groups = lsort $ groupBy equalLength $ lsort lists
          equalLength xs ys = length xs == length ys
