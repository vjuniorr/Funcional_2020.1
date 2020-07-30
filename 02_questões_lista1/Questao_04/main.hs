fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = 
    fibonacci (x - 1) + fibonacci(x - 2)

main = do
    line1 <- getLine

    let x = read line1 :: Int

    print $ fibonacci x 