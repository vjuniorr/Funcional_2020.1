elemento y xs = 
    xs !! y
    
main = do
    line1 <- getLine
    line2 <- getLine

    let y = read line2 :: Int   

    print $ elemento y [read x :: Int | x <- words line1]