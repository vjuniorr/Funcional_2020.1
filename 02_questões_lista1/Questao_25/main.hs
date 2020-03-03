isSorted xs =
    if(null xs)
        then True
        else verifica (head xs) (tail xs)
verifica x ys =
    if(null ys)
        then True
        else if(x > (head ys))
            then False
            else verifica (head ys) (tail ys) 

main = do
    line1 <- getLine

    print $ isSorted [read x :: Int | x <- words line1]