intercal [] (y:ys) = [] ++ y
intercal (x:xs) [] = x ++[]
intercal (x:xs) (y:ys) =
    x ++ y ++ intercal xs ys