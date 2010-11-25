---- didn't know how to generate random numbers in Haskell, copied from the wiki

module P23 where

import System.Random (randomRIO)
import Control.Monad (replicateM)

-- this is elegent but doesn't solve the original problem, which means you should select each element at most once.
rnd_select :: [a] -> Int -> IO [a]
rnd_select xs n = replicateM n rand
    where rand = do r <- randomRIO (0, (length xs) - 1)
                    return $ xs!!r

---- this is my own implementation
rnd_select' :: [a] -> Int -> IO [a]
rnd_select' _ 0 = return []
rnd_select' (x:xs) n =
    do r <- randomRIO (0, (length xs))
       if r < n
           then do
               rest <- rnd_select' xs (n-1)
               return (x : rest)
           else rnd_select' xs n
