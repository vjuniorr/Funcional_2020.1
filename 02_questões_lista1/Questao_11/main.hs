maioresQue y xs = 
    filter (y <) xs
    
main = do
    line1 <- getLine
    line2 <- getLine
    let y = read line2 :: Int   

    print $ maioresQue y [read x :: Int | x <- words line1]