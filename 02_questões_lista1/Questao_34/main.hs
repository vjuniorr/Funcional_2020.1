sdig 0 = 0
sdig x = 
    (x `mod` 10) + sdig (x `div` 10)