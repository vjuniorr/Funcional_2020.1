fatorial 0 = 1
fatorial x =
    x * fatorial(x - 1)
            
main = do
    line1 <- getLine

    let x = read line1 :: Int

    print $ fatorial x 