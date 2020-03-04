menorDeTres x y z =
    if z < y && z < x 
        then z
        else if x < y && x < z
            then x
            else y
            
main = do
    line1 <- getLine
    line2 <- getLine
    line3 <- getLine

    let x = read line1 :: Int
    let y = read line2 :: Int
    let z = read line3 :: Int

    print $ menorDeTres x y z