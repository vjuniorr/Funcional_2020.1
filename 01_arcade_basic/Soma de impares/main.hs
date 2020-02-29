somaImpares xs =
    sum (filter odd xs) 
main = do
    line <- getLine
    print $ somaImpares [read x :: Int | x <- words line]