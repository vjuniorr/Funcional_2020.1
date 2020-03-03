frequencia xs x =
    if(null xs)
        then 0
        else if((head xs) == x)
            then 1 + frequencia (tail xs) x
            else 0 + frequencia (tail xs) x
main = do
    line1 <- getLine
    line2 <- getLine

    let y = read line2 :: Int   

    print $ frequencia [read x :: Int | x <- words line1] y