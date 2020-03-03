total xs = 
    sum [1 | _ <- xs]

main = do
    line1 <- getLine   

    print $ total [read x :: Int | x <- words line1] 