and'  a b = a && b
or'   a b = a || b
equ'  a b = a == b
not'  a   = not a
nand' a b = not' (and' a b)
nor'  a b = not' (or' a b)
xor'  a b = not' (equ' a b)
impl' a b = or' (not' a) b

----

table :: (Bool -> Bool -> Bool) -> IO ()
table f = mapM_ putStrLn
          [ show a ++ " " ++ show b ++ " " ++ " " ++ show (f a b)
          | a <- [True, False], b <- [True, False] ]
