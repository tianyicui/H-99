repli xs n = concatMap (replicate n) xs

---- Pointfree style

repli' = flip $ concatMap . replicate
