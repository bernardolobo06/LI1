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

== __Notas:__
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
remove ::
  [String]      -- ^ argumento
  -> Char       -- ^ argumento
  -> [String]   -- ^ resultado
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
addTuple ::
  [(Int, Int)]      -- ^ argumento
  -> Int            -- ^ argumento
  -> [(Int, Int)]   -- ^ resultado
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
maxSndTuple ::
  [(Int, Int)]      -- ^ argumento (lista não vazia)
  -> Int            -- ^ resultado
maxSndTuple [(a, b)] = b
maxSndTuple (h:t) = max (snd h) (maxSndTuple t)


-----continuar a escrever a DOCUMENTAÇÃO a partir deste ponto-----

type Nome = String
type Coordenada = (Double, Double)
data Movimento = N | S | E | W deriving (Show,Eq) -- Norte, Sul, Este, Oeste
type Movimentos = [Movimento]
data PosicaoPessoa = Pos Nome Coordenada deriving (Show,Eq)

-------exercicio extra: criar funcao auxiliar para executar os movimentos, ao invés de fazer por casos--------

--assume-se que o movimento é de uma unidade
posicoesM :: [PosicaoPessoa] -> Movimento -> [PosicaoPessoa]
posicoesM [] _ = []
posicoesM ((Pos name (x, y)):t) N = Pos name (x, y+1) : posicoesM t N
posicoesM ((Pos name (x, y)):t) S = Pos name (x, y-1) : posicoesM t S
posicoesM ((Pos name (x, y)):t) E = Pos name (x+1, y) : posicoesM t E
posicoesM ((Pos name (x, y)):t) W = Pos name (x-1, y) : posicoesM t W

posicao :: PosicaoPessoa -> Movimentos -> PosicaoPessoa
posicao pos [] = pos
posicao (Pos name (x, y)) (h:t)
  | h == N = posicao (Pos name (x, y+1)) t
  | h == S = posicao (Pos name (x, y-1)) t
  | h == E = posicao (Pos name (x+1, y)) t
  | h == W = posicao (Pos name (x-1, y)) t

posicoesMs :: [PosicaoPessoa] -> Movimentos -> [PosicaoPessoa]
posicoesMs [] _ = []
posicoesMs (h:t) mov = posicao h mov : posicoesMs t mov