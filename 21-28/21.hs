insertAt x xs pos =
    let (first,second) = splitAt (pos-1) xs
    in first++(x:second)
