---- didn't know how to generate random numbers in Haskell

import System.Random
import Control.Monad (replicateM)

rnd_select :: [a] -> Int -> IO [a]
rnd_select [] _ = return []
rnd_select xs n = do
    pos <- replicateM n $ getStdRandom $ randomR (0, (length xs)-1)
    return [xs!!p | p <- pos]
