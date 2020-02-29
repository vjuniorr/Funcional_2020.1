final n xs =
    drop (length xs - n) xs
main = do
    qtd <- readLn :: IO Int
    line <- getLine
    print $ final qtd [read x :: Int | x <- words line]