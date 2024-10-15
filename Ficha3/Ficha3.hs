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
>>> posicao [(Pos "Afonso" (0, 0)), (Pos "Bernardo" (5, -5))] [N, N, E, S]
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

------------------------------------------------------------------------------------------------

{-| A função 'elem2' recebe uma lista e desloca cada elemento da lista, n posições para a esquerda.

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
elem2 :: Eq a => [a] -> a -> Int
elem2 [] _ = -1
elem2 (h:t) el
  | h /= el = 1 + elem2 t el
  | otherwise = 0

  -- se não entregar uma lista vazia mas o elemento não estiver lá, não dá -1







  --exercicio 8 na ficha 4