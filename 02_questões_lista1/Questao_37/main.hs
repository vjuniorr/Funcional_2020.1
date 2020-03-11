splitints xs =
    ([x | x<-xs, x `mod` 2 /= 0], [x | x<-xs, x `mod` 2 == 0])