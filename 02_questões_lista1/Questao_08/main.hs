maior xs = 
    maioraux xs 0
maioraux ys z =
    if(null ys)
        then z
        else if((head ys) > z)
            then maioraux (tail ys) (head ys)
            else maioraux (tail ys) z

main = do
    line1 <- getLine   

    print $ maior [read x :: Int | x <- words line1] 