isPallid str =
    if str == reverse str
        then True
        else False
--Nao consegui fazer a main para pegar a string
--main = do
--    line <- getLine
--    let str = read line :: String
--
--    isPallid str