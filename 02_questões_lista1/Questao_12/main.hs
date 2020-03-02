concatena xs ys =
    xs ++ ys
    
main = do
    line1 <- getLine
    line2 <- getLine
    
    print $ concatena [read x :: Int | x <- words line1] [read y :: Int | y <- words line2]