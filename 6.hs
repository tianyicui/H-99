isPalindrome xs = (xs == (reverse xs))

isPalindrome' []  = True
isPalindrome' [x] = True
isPalindrome' xs  = (head xs) == (last xs) && (isPalindrome' $ init $ tail xs)
