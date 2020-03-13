isSorted [] = True
isSorted [x] = True
isSorted (x:xs)
    | x <= head xs = isSorted xs
    | otherwise = False
