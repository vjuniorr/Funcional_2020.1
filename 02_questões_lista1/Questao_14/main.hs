corpo xs = init 
    
main = do
    line1 <- getLine
    
    print $ corpo [read x :: Int | x <- words line1] 