duasIguais [] [] = True
duasIguais xs [] = False
duasIguais [] ys = False
duasIguais (x:xs) (y:ys) =
    if (x == y)
        then duasIguais xs ys
        else False
