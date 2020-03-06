menores x xs =
    [y | y <- xs, y <= x]

main = do
    line1 <- getLine
    line2 <- getLine

    let y = read line2 :: Int   

    print $ menores y [read x :: Int | x <- words line1]