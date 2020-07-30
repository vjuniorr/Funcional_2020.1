frequencia x [] = 0
frequencia x xs 
    | head xs == x = 1 + frequencia x (tail xs)
    | otherwise = frequencia x (tail xs)

unico x xs 
    | frequencia x xs == 1 = True
    | otherwise = False
        
main = do
    line1 <- getLine
    line2 <- getLine

    let y = read line2 :: Int   

    print $ unico [read x :: Int | x <- words line1] y