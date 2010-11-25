gray 0 = [""]
gray n =
    (map ('0':) $ gray (n-1)) ++
    (map ('1':) $ reverse $ gray (n-1))
