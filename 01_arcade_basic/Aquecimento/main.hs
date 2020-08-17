-- countNeg -- 
countNeg xs = 
    length(filter(0>) xs)

-- final --
final x xs =
    drop x xs

-- iguais -- 
iguais x y z 
    | x == y && y == z = 3
    | x == y && y /= z || x == z && z /= y || y == z && z /= x = 2
    | otherwise = 0

-- interior -- 
interior xs =
    init (drop 1 xs)

-- gangorra -- 
gangorra p1 c1 p2 c2
    | (p1 * c1) == (p2 * c2) = 0
    | (p1 * c1) > (p2 * c2) = -1
    | otherwise = 1

-- min2 -- 
min2 x y 
    | x > y = y
    | otherwise = x