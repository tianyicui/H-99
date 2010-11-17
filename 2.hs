myButLast [x,_]  = x
myButLast (_:xs) = myButLast xs

-- Why can't `!!` support negative number like Python/Ruby do?
myButLast' x = reverse x !! 1

-- First time to know `.`
myButLast'' = last . init
