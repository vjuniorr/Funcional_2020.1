
inserir y (x:xs)
    | x < y && y < head xs = [x] ++ [y] ++ xs
    | x > y = y : (x:xs)
    | last xs < y = (x:xs) ++ [y]
    | otherwise = inserir y xs
    