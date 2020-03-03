frequencia xs x =
    if(null xs)
        then 0
        else if((head xs) == x)
            then 1 + frequencia (tail xs) x
            else 0 + frequencia (tail xs) x
unico ys n =
    if (frequencia ys n == 1)
        then True
        else False
main = do
    line1 <- getLine
    line2 <- getLine

    let y = read line2 :: Int   

    print $ unico [read x :: Int | x <- words line1] y