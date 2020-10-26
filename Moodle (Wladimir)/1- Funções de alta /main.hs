-- concatenaFold --
concatenaFold :: [[Int]] -> [Int]
concatenaFold [] = []
concatenaFold xs = foldl(++)[] xs

{- 
    Função que vai concatenar uma lista de listas em uma unica lista 
    O foldl vai concatenar em uma lista vazia todos os elementos de uma lista de listas
    Então uma [[],[],[]] vai ficar -> [..]
 -}

-- inverteFold --
inverteFold :: [Int] -> [Int]
inverteFold xs = foldl (flip(:)) [] xs

{-
    Função que inverte uma lista de inteiros
    O foldl vai preencher uma lista vazia com o inverso da lista xs
    A função flip vai fazer com que o fold preencha a lista vazia com o inverso de xs
    Então uma lista [1,2,3,4,5] -> [5,4,3,2,1]
 -}

-- paridadeFold --
paridadeFold :: [Bool] -> Bool
paridadeFold xs
    | length(filter (==True) xs) `mod` 2 == 0 = True 
    | otherwise = False

{- 
    Função que retorna True se minha lista de Bool tiver um numero par de True e False caso contrario
    Eu filtro a de modo que eu pego os elementos que forem True, depois pego o tamanho da lista com esse filtro e verifico se é par 
    Se for par retorno True 
    Outro caso eu retorno False
    Então [True, False, False, True, True] -> False e [True, False, False, True] -> True
 -}

-- duplicarFold -- 
duplicarFold :: String -> String
duplicarFold [] = []
duplicarFold (s:str) = 
    let xs = ['a', 'e', 'i', 'o', 'u', 'A', 'E', 'I', 'O', 'U']
    in 
        if s `elem` xs then 
                s : filter(== s) xs ++ duplicarFold str
            else 
                s : duplicarFold str

{-
    Função que retorna uma string com vogais repetidas
    Eu pego a cabeça da string(s) e crio uma lista xs que contem as vogais minusculas e maiusculas
    Verifico se s é elemento de xs, se for eu concateno s com o elemento igual de xs com o restante da string
    Então "Ana" -> "AAnaa"
 -}

-- filtraAplicaFold --
filtraAplicaFold :: (a->b) -> (a->Bool) -> [a] -> [b]
filtraAplicaFold f p xs = 
    map f (filter p xs)

{- 
    Função que vai aplicar uma função em determinados elementos da minha lista 
    Crio uma lista com um map aplicando f na lista xs que vai ser filtrada por p
    então map (*2) (==3) [1,2,3] -> [6]
 -}

-- mapFold --
mapFold :: Foldable t1 => (t2 -> a) -> t1 t2 -> [a]
mapFold f xs = foldr (\x z -> f x : z) [] xs 

{-
    Função que vai aplicar f em todos os elementos de xs
    O foldr vai preencher a lista vazia com a função passada em cada elemento de xs
    Então (*4) [1,2,3,4] -> [4,8,12,16]
 -}

-- removeLista --
removeLista :: (Foldable t1, Foldable t2, Eq a) => t2 a -> t1 a -> [a]
removeLista xs ys = foldr (\x z-> if x `notElem` xs then x:z else z ) [] ys

{-
    Função que vai remover os elementos de ys que estão em xs
    O foldr vai percorrer os elementos de ys (x) e verificar se ele não é elemento de xs, se não for ele concatena com o restante (z) na lista vazia
    Então [1,2,3] [1,2,3,4,5,6,7] -> [4,5,6,7]
 -}
-- acertosFold --
{- acertosFold xs ys = foldr g v [0..tam-1]

  where

  v = 0  

  tam = length xs  -}

-- descompactaFold -- 
descompactaFold :: Foldable t => t (a1, a2) -> ([a1], [a2])
descompactaFold xs = 
    foldr (\(a,b) (xs,ys) -> ([a]++xs, [b]++ys)) ([],[]) xs

{-
    Função que vai descompactar uma lista de tupla e concatena os primeiros elementos com todos os primeiros e segundos com todos os segundos
    O foldr vai percorrer os elementos de xs ((a,b)) e concatena com os proximos colocando em uma lista vaiza
    Então [(1,2),(3,4)] -> ([1,3],[2,4])
 -}
