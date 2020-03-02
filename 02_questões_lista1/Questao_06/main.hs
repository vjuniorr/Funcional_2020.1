pertence y xs =
    y `elem` xs
    
main = do
    line1 <- getLine
    line2 <- getLine

    let y = read line2 :: Int   

    print $ pertence y [read x :: Int | x <- words line1]