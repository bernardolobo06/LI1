{-|
Module      : Ficha 3
Description : Módulo Haskell contendo funções recursivas.
Copyright   : (c) Marcos Bernardo da Silva Lobo <a110959@alunos.uminho.pt>, 2024
              Olga Maria Gomes Martins Pacheco <omp@di.uminho.pt>, 2024   
Maintainer  : a110959@alunos.uminho.pt
Stability   : Experimental
Portability : POSIX (Portable Operating System Interface)

Este módulo contém as definições Haskell, para o cálculo de funções recursivas sobre listas e sobre inteiros, pedidas na Ficha 3 de Laboratórios de Informática I.
-}

module Ficha3 where

{-| A função 'remove' recebe uma lista de strings e remove todas as strings iniciadas por um dado caractere.

==  __Notas:__
* Esta função é sensível a maiúsculas e minúsculas;
* Esta função funciona para caracteres numéricos;
* Esta função funciona para caracteres especiais;
* Esta função funciona para listas vazias.

==  __Exemplos de utilização:__
>>> remove ["Ola", "Amor", "Comida"] 'A'
["Ola", "Comida"]
>>> remove ["Ola", "Amor", "Comida"] 'a'
["Ola", "Amor", "Comida"]
>>> remove [] 'a'
[]

== __Propriedades:__
prop> remove [] _ = []
-}
remove :: [String] -> Char -> [String]
remove [] _ = []
remove (h:t) char
    | head h /= char = h : remove t char
    | otherwise = remove t char


{-| A função 'addTuple' recebe uma lista de pares de inteiros e adiciona um valor dado à primeira componente de cada par.

==  __Exemplos de utilização:__
>>> addTuple [(1, 2), (-1, 1), (0, 0)] 5
[(6, 2), (4, 1), (5, 0)]
>>> addTuple [(1, 2), (-1, 1), (0, 0)] (-1)
[(0, 2), (-2, 1), (-1, 0)]
>>> addTuple [(1, 2), (-1, 1), (0, 0)] 0
[(1, 2), (-1, 1), (0, 0)]
>>> addTuple [] 9
[]

== __Propriedades:__
prop> addTuple [] _ = []
-}
addTuple :: [(Int, Int)] -> Int -> [(Int, Int)]
addTuple [] _ = []
addTuple (h:t) num = (fst h + num, snd h) : addTuple t num


{-| A função 'maxSndTuple' recebe uma lista, não vazia, de pares de inteiros e calcula qual o maior valor da segunda componente.

==  __Exemplos de utilização:__
>>> maxSndTuple [(1, 2), (-1, 1), (0, 0)]
2
>>> maxSndTuple [(1, 2), (-1, 1), (0, 3)]
3

== __Propriedades:__
prop> maxSndTuple [(a, b)] = b
-}
maxSndTuple :: [(Int, Int)] -> Int
maxSndTuple [(a, b)] = b
maxSndTuple (h:t) = max (snd h) (maxSndTuple t)


-- | O @type@ 'Nome' é sinónimo de uma @string@ (representativa de uma identidade).
type Nome = String
-- | O @type@ 'Coordenada' é sinónimo de um par de @doubles@ (representativo de um conjunto de coordenadas cartesianas).       
type Coordenada = (Double, Double)
-- | O @data type@ 'Movimento' é a definição de um novo tipo de dados (representativo de um movimento segundo os pontos cardeais).     
data Movimento = N  -- ^(North) --> y + 1
               | S  -- ^(South) --> y - 1
               | E  -- ^(East) --> x + 1
               | W  -- ^(West) --> x - 1
              deriving (Show,Eq)
-- | O @type@ 'Movimentos' é sinónimo de uma lista do tipo 'Movimento' (representativa de uma sequência finita de movimentos).       
type Movimentos = [Movimento]
-- | O @data type@ 'PosicaoPessoa' é a definição de um novo tipo de dados (representativo de um conjunto de coordenadas cartesianas referentes a uma identidade).               
data PosicaoPessoa = Pos Nome Coordenada deriving (Show,Eq)


{-| A função 'auxCoord' recebe um @data type@ 'PosicaoPessoa' e um 'Movimento' e calcula novas coordenadas.

==  __Exemplos de utilização:__
>>> auxCoord (Pos "Afonso" (0, 0)) N
(Pos "Afonso" (0, 1))
>>> auxCoord (Pos "Afonso" (0, 0)) S
(Pos "Afonso" (0, -1))
>>> auxCoord (Pos "Afonso" (0, 0)) E
(Pos "Afonso" (1, 0))
>>> auxCoord (Pos "Afonso" (0, 0)) W
(Pos "Afonso" (-1, 0))
-}
auxCoord :: PosicaoPessoa -> Movimento -> PosicaoPessoa
auxCoord (Pos name (x, y)) N = Pos name (x, y + 1)
auxCoord (Pos name (x, y)) S = Pos name (x, y - 1)
auxCoord (Pos name (x, y)) E = Pos name (x + 1, y)
auxCoord (Pos name (x, y)) W = Pos name (x - 1, y)


{-| A função 'posicoesM' recebe uma lista de posições de pessoas e um movimento e atualiza as coordenadas de todas as posições.

==  __Notas:__
* Esta função recorre à anterioremente definida 'auxCoord'.
* Assume-se que o movimento corresponde a uma unidade do referencial.

==  __Exemplos de utilização:__
>>> posicoesM [(Pos "Afonso" (0, 0)), (Pos "Bernardo" (5, 3))] N
[(Pos "Afonso" (0, 1)), (Pos "Bernardo" (5, 4))]
>>> posicoesM [(Pos "Afonso" (0, 0)), (Pos "Bernardo" (5, 3))] S
[(Pos "Afonso" (0, -1)), (Pos "Bernardo" (5, 2))]
>>> posicoesM [(Pos "Afonso" (0, 0)), (Pos "Bernardo" (5, 3))] E
[(Pos "Afonso" (1, 0)), (Pos "Bernardo" (6, 3))]
>>> posicoesM [(Pos "Afonso" (0, 0)), (Pos "Bernardo" (5, 3))] W
[(Pos "Afonso" (-1, 0)), (Pos "Bernardo" (4, 3))]

== __Propriedades:__
prop> posicoesM [] _ = []
-}
posicoesM :: [PosicaoPessoa] -> Movimento -> [PosicaoPessoa]
posicoesM [] _ = []
posicoesM (h:t) mov = auxCoord h mov : posicoesM t mov


{-| A função 'posicao' recebe um @data type@ 'PosicaoPessoa' e uma lista de movimentos e calcula a posição da pessoa depois de executar todos os movimentos.

==  __Notas:__
* Esta função recorre à anterioremente definida 'auxCoord'.
* Assume-se que o movimento corresponde a uma unidade do referencial.

==  __Exemplos de utilização:__
>>> posicao (Pos "Afonso" (0, 0)) [S, S, W]
Pos "Afonso" (-1, -2)
>>> posicao (Pos "Afonso" (0, 0)) [N, N, E, S]
Pos "Afonso" (1, 1)

== __Propriedades:__
prop> posicao pos [] = pos
-}
posicao :: PosicaoPessoa -> Movimentos -> PosicaoPessoa
posicao pos [] = pos
posicao pos (h:t) =  auxCoord pos h `posicao` t


{-| A função 'posicoesMs' recebe uma lista de posições de pessoas e uma lista de movimentos e calcula a posição de cada pessoa depois de executar todos os movimentos.

==  __Notas:__
* Esta função recorre à anterioremente definida 'auxCoord'.
* Esta função recorre à anterioremente definida 'posicao'.
* Assume-se que o movimento corresponde a uma unidade do referencial.

==  __Exemplos de utilização:__
>>> posicoesMs [(Pos "Afonso" (0, 0)), (Pos "Bernardo" (5, -5))] [S, S, W]
[(Pos "Afonso" (-1, -2)), (Pos "Bernardo" (4, -7))]
>>> posicoesMs [(Pos "Afonso" (0, 0)), (Pos "Bernardo" (5, -5))] [N, N, E, S]
[(Pos "Afonso" (1, 1)), (Pos "Bernardo" (6, -4))]

== __Propriedades:__
prop> posicoesMs [] _ = []
-}
posicoesMs :: [PosicaoPessoa] -> Movimentos -> [PosicaoPessoa]
posicoesMs [] _ = []
posicoesMs (h:t) mov = posicao h mov : posicoesMs t mov


{-| A função 'pessoaNorte' recebe uma lista de posições de pessoas e indica qual a posição mais a Norte.

==  __Notas:__
* Assume-se que a pessoa mais a Norte é aquela cuja ordenada é maior.

==  __Exemplos de utilização:__
>>> pessoaNorte [(Pos "Afonso" (0, 0)), (Pos "Bernardo" (5, -5))]
Just (Pos "Afonso" (0, 0))
>>> pessoaNorte [(Pos "Afonso" (0, 0)), (Pos "Bernardo" (5, 5))]
Just (Pos "Bernardo" (5, 5))

== __Propriedades:__
prop> pessoaNorte [] = Nothing
prop> pessoaNorte [a] = Just a
-}
pessoaNorte :: [PosicaoPessoa] -> Maybe PosicaoPessoa
pessoaNorte [] = Nothing
pessoaNorte [a] = Just a
pessoaNorte ((Pos name1 (x1, y1)) : (Pos name2 (x2, y2)) : t)
  | y1 > y2 = pessoaNorte (Pos name1 (x1, y1) : t)
  | otherwise = pessoaNorte (Pos name2 (x2, y2) : t)


{-| A função 'pessoasNorte' recebe uma lista de posições de pessoas e indica quais as pessoas mais a Norte.
Ou seja, uma lista com o nome de todas as pessoas, exceto a(s) que se encontre(m) mais a Sul.

==  __Notas:__
* Assume-se que a pessoa mais a Sul é aquela cuja ordenada é menor.
* Caso exista apenas um elemento na lista, o resultado terá esse elemento.

==  __Exemplos de utilização:__
>>> pessoasNorte [(Pos "Afonso" (0, 0)), (Pos "Bernardo" (5, -5)), (Pos "Carlos" (5, -5))]
["Afonso"]
>>> pessoasNorte [(Pos "Afonso" (0, 0)), (Pos "Bernardo" (5, 5)), (Pos "Carlos" (5, -5))]
["Bernardo", "Afonso"]
>>> pessoasNorte [(Pos "Afonso" (0, 0)), (Pos "Bernardo" (5, 0)), (Pos "Carlos" (-5, 0))]
[]

== __Propriedades:__
prop> pessoasNorte [] = []
prop> pessoasNorte [Pos name (x, y)] = [name]
-}
pessoasNorte :: [PosicaoPessoa] -> [String]
pessoasNorte [] = []
pessoasNorte [Pos name _] = []
pessoasNorte ((Pos name1 (x1, y1)) : (Pos name2 (x2, y2)) : t)
  | y1 < y2 = name2 : pessoasNorte (Pos name1 (x1, y1) : t)
  | y1 > y2 = name1 : pessoasNorte (Pos name2 (x2, y2) : t)
  | otherwise = pessoasNorte (Pos name1 (x1, y1) : t)


{-| A função 'nDir' recebe uma lista e desloca cada elemento da lista, n posições para a direita.

==  __Notas:__
* Apenas funciona se o @n@ for menor ou igual ao comprimento da lista.
* Na utilização de um @n@ superior ao comprimento da lista é devolvida a própria lista.
* Na utilização de um @n@ igual ou menor que 0 é devolvida a própria lista.

==  __Exemplos de utilização:__
>>> nDir [1, 2, 3, 4, 5] 6
[1, 2, 3, 4, 5]
>>> nDir [1, 2, 3, 4, 5] 2
[4, 5, 1, 2, 3]
>>> nDir [1, 2, 3, 4, 5] 0
[1, 2, 3, 4, 5]
>>> nDir [1, 2, 3, 4, 5] -2
[1, 2, 3, 4, 5]

== __Propriedades:__
prop> nDir [] _ = []
-}
nDir :: [a] -> Int -> [a]
nDir [] _ = []
nDir (h:t) n
  | length (h:t) - n > 0 = nDir (t ++ [h]) (n+1)
  | otherwise = h:t


{-| A função 'nEsq' recebe uma lista e desloca cada elemento da lista, n posições para a esquerda.

==  __Notas:__
* Apenas funciona se o @n@ for menor ou igual ao comprimento da lista.
* Na utilização de um @n@ superior ao comprimento da lista é devolvida a própria lista.
* Na utilização de um @n@ igual ou menor que 0 é devolvida a própria lista.

==  __Exemplos de utilização:__
>>> nEsq [1, 2, 3, 4, 5] 6
[1, 2, 3, 4, 5]
>>> nEsq [1, 2, 3, 4, 5] 2
[3, 4, 5, 1, 2]
>>> nEsq [1, 2, 3, 4, 5] 0
[1, 2, 3, 4, 5]
>>> nEsq [1, 2, 3, 4, 5] -2
[1, 2, 3, 4, 5]

== __Propriedades:__
prop> nEsq [] _ = []
-}
nEsq :: [a] -> Int -> [a]
nEsq [] _ = []
nEsq (h:t) n
  | n > 0 = nEsq (t ++ [h]) (n-1)
  | otherwise = h:t


{-| A função 'search' procura a posição da primeira ocorrência de um elemento numa lista.

==  __Notas:__
* Devolve (-1) caso o elemento não ocorra na lista.
* O índice de posições começa em zero.

==  __Exemplos de utilização:__
>>> search [1, 2, 3, 4, 5] 6
-1
>>> search [] 2
-1
>>> search [1, 2, 3, 4, 5] 4
2
>>> search [1, 2, 3, 4, 5] 1
0

== __Propriedades:__
prop> search [] _ = -1
-}
search :: Eq a => [a] -> a -> Int
search l el = aux l el 0
  where aux [] _ _ = -1
        aux (h:t) el i | h == el = i
                       | otherwise = aux t el (i+1)


{-| A função 'swap' troca o elemento de uma determinada posição numa lista, por outro elemento dado.

== __Notas:__
* Se a lista estiver vazia, devolve a própria lista.
* Se a lista não tiver tantos elementos quanto o índice dado, devolve a própria lista.

== __Exemplos de utilização:__
>>> swap 'X' 3 "abcdefg"
"abcXefg"
>>> swap 10 7 [1..5]
[1, 2, 3, 4, 5]
>>> swap 10 5 []
[]

== __Propriedades:__
prop> swap _ _ [] = []
-}
swap :: a -> Int -> [a] -> [a]
swap el i l = aux el i l 0
  where aux _ _ [] _ = []
        aux el i (h:t) count | count == i = el:t
                             | otherwise = h:aux el i t (count+1)


{-| A função 'listIndex'' percorre uma lista até um índice dado e devolve o elemento desse índice.

== __Notas:__
* Esta função não funciona para listas vazias.
* O índice de posições começa em 0.
* O índice dado deve ser inferior ao comprimento da lista.

== __Exemplos de utilização:__
>>> listIndex' [0..9] 5
5
>>> listIndex' ['a'..'f'] 2
'c'
-}
listIndex' :: [a] -> Int -> a
listIndex' l i = aux l i 0
  where aux (h:t) i count | i == count = h
                          | otherwise = aux t i (count+1)


{-| A função 'concat'' concatena uma lista de listas, ou seja, une o conteúdo de todas as listas numa só.

== __Notas:__
* Se a lista estiver vazia, devolve a própria lista. 
* A ordem das listas é conservada.

== __Exemplos de utilização:__
>>> concat' [[0..4], [2..5]]
[0,1,2,3,4,2,3,4,5]
>>> concat' ["Hello", "World!"]
"HelloWorld!"

== __Propriedades:__
prop> concat' [] = []
-}
concat' :: [[a]] -> [a]
concat' [] = []
concat' (h:t) = h ++ concat' t


{-| A função 'words'' recebe uma string, e devolve uma lista de strings com todas as palavras da string original, sem espaços.

== __Exemplos de utilização:__
>>> words' []
[""]
>>> words' "Hello World"
["Hello","World"]
>>> words' "Hi, I am Haskell"
["Hi,","I","am","Haskell"]

== __Propriedades:__
prop> words' [] = [""]
-}
words' :: [Char] -> [[Char]]
words' l = aux l []
  where aux [] ls = [ls]
        aux (h:t) ls | h /= ' '  = aux t (ls ++ [h])
                     | otherwise = ls : aux t []


{-| A função 'unwords'' recebe uma lista string, e devolve a união das strings originais, espaçadas.

== __Exemplos de utilização:__
>>> unwords' [""]
""
>>> unwords' ["Hello","World"]
"Hello World"
>>> unwords' ["Hi,","I","am","Haskell"]
"Hi, I am Haskell"

== __Propriedades:__
prop> unwords' [a] = a
-}
unwords' :: [[Char]] -> [Char]
unwords' [a] = a
unwords' (h:t) = h ++ ' ' : unwords' t