rotEsq n xs =
    let ys = take (n `mod` length xs) xs
        zs = drop (n `mod` length xs) xs
    in
        zs ++ ys