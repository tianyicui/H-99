myLast [x]    = x
myLast (_:xs) = myLast xs

----

-- XXX: Did not quite get it
myLast' = foldr1 (const id)
