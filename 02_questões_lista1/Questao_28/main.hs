rotDir n xs =
    let ys = take (length xs - n) xs
        zs = drop (length xs - n) xs
    in 
        zs ++ ys
