---- didn't know how to generate random numbers in Haskell, copied from the wiki

import System.Random
import Control.Monad (replicateM)

rnd_select :: [a] -> Int -> IO [a]
rnd_select xs n = replicateM n rand
    where rand = do r <- randomRIO (0, (length xs) - 1)
                    return $ xs!!r
