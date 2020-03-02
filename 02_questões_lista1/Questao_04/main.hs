fibonacci x =
    if x == 0 || x == 1
        then x
        else fibonacci (x - 1) + fibonacci (x - 2)

main = do
    line1 <- getLine

    let x = read line1 :: Int

    print $ fibonacci x 