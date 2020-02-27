max3 x y z =
    if x > y
        then if x > z
            then x
            else z
        else if y > z
            then y
            else z    
main = do
    line1 <- getLine 
    line2 <- getLine
    line3 <- getLine

    let x = read line1 :: Int
    let y = read line2 :: Int
    let z = read line3 :: Int

    print $ max3 x y z