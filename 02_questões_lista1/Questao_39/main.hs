base 0 b = ""
base n b =
    base (n `div` b) b ++ [bases !! (n `mod` b)]
    where bases = ['0' .. '9'] ++ ['A' .. 'Z']

