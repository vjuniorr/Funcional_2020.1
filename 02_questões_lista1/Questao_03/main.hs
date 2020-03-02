fatorial x =
    if x == 0
        then 1
        else x * fatorial (x - 1)
        
main = do
    line1 <- getLine

    let x = read line1 :: Int

    print $ fatorial x 