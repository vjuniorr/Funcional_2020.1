menorDeDois x y =
    if x > y
        then y
        else x
        
main = do
    line1 <- getLine
    line2 <- getLine

    let x = read line1 :: Int
    let y = read line2 :: Int

    print $ menorDeDois x y