calda xs =
    tail xs
    
main = do
    line1 <- getLine
    
    print $ calda [read x :: Int | x <- words line1] 