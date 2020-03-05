compara' [] [] = "0"
compara' xs [] = "-1"
compara' [] ys = "1"
compara' (x:xs) (y:ys) =
    if(x == y)
        then compara' xs ys
        else False
