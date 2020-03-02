reverso xs =
    reverse xs

main = do
    line <- getLine

    print $ reverso [read x :: Int | x <- words line]