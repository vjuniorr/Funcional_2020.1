selec str [] = []
selec [] ls = []
selec str ls =
    (str!!head ls):selec str (tail ls)