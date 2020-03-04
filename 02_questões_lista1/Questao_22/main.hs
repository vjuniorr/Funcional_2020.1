intersec xs ys =
    [x | x <- xs, (x `elem` xs) && (x `elem` ys)]

main = do
    line1 <- getLine
    line2 <- getLine
    
    print $ intersec [read x :: Int | x <- words line1] [read y :: Int | y <- words line2]