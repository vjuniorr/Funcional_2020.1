interior xs = 
    init (drop 1 xs) 
main = do
    line <- getLine
    print $ interior [read x :: Int | x <- words line]