sequencia n m =
    [m .. (m + n - 1)]
main = do
    line1 <- getLine
    line2 <- getLine
    let n = read line1 :: Int
    let m = read line2 :: Int

    print $ sequencia n m