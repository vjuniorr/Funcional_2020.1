primo x =
    if((verifica [1 .. x] x) == 2)
        then True
        else False
verifica xs y =
    if(null xs)
        then 0
        else if((y `mod` (head xs)) == 0)
            then 1 + verifica (tail xs) y
            else 0 + verifica (tail xs) y

main = do 
    line <- getLine
    let x = read line :: Int

    print $ primo x

