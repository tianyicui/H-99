import Control.Monad (replicateM)

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
          [ show a ++ " " ++ show b ++ " " ++ show (f a b)
          | a <- [True, False], b <- [True, False] ]

tablen :: Int -> ([Bool] -> Bool) -> IO ()
tablen n f =
    mapM_ putStrLn
    [ showList $ eval xs | xs <- values n] where
        values 0 = [[]]
        values n = [ True  : l | l <- values $ n-1 ] ++
                   [ False : l | l <- values $ n-1 ]
        eval xs = xs ++ [f xs]
        showList [x]    = show x
        showList (x:xs) = show x ++ " " ++ showList xs

----

tablen' :: Int -> ([Bool] -> Bool) -> IO ()
tablen' n f =
    mapM_ putStrLn
    [ unwords $ map show $ xs ++ [f xs]
    | xs <- replicateM n [True, False] ]
