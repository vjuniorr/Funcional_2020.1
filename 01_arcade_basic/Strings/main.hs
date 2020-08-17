-- upper --
import Data.Char
upper s =
    [toUpper c | c <- s]

-- selec -- 
selec str [] = []
selec [] ls = []
selec str ls = 
    (str!!head ls):selec str (tail ls)

-- isPalind -- 
isPalind str 
    | str == reverse str = True
    | otherwise = False