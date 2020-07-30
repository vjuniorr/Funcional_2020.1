maioresQue x [] = []
maioresQue x xs 
    | x < head xs = head xs:maioresQue x (tail xs)
    | otherwise = maioresQue x (tail xs)
    
main = do
    line1 <- getLine
    line2 <- getLine
    let y = read line2 :: Int   

    print $ maioresQue y [read x :: Int | x <- words line1]