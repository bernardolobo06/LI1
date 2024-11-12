module Miniteste2 where

import Test.HUnit


{-| A função 'f' recebe uma lista e um elemento e devolve o índice do elemento na lista.

== __Notas:__
* Se o elemento não existir na lista, a função devolve 'Nothing'.
* Se o elemento existir na lista, a função devolve 'Just n', sendo 'n' o índice.Applicative

== __Exemplos de utilização:__
>>> f ['a'..'f'] 'c'
Just 2
>>> f ['a'..'f'] 'g'
Nothing

== __Propriedades:__
prop> f [] _ = Nothing
-}
f :: Eq a => [a] -> a -> Maybe Int
f l x = procura x li
    where li = zip l [0..]
          procura x [] = Nothing
          procura x ((y,n):ys) | x==y = Just n
                               | otherwise = procura x ys

testeF1 = "Teste f [0..9] 5"  ~: Just 5  ~=? f [0..9] 5
testeF2 = "Teste f [0..9] 10" ~: Nothing ~=? f [0..9] 10
testesF = test [testeF1, testeF2]


{-| A função 'g' recebe uma lista de listas de nomes, e devolve uma lista de pares nome-posição.

== __Notas:__
* A ordenação das listas baseia-se no processo de concatenação.

== __Exemplos de utilização:__
>>> g [["Ana", "Beatriz"], ["Claudia", "Diana"], ["Eduarda", "Francisca"]]
[("Ana",1),("Beatriz",2),("Claudia",3),("Diana",4),("Eduarda",5),("Francisca",6)]
>>> g []
[]

== __Propriedades:__
prop> g [] = []
-}
g :: [[String]] -> [(String, Int)]
g l = zip (aux l) [1..]
    where aux [] = []
          aux (h:t) = h ++ aux t

testeG1 = "Teste g [['Ana', 'Beatriz'], ['Claudia', 'Diana'], ['Eduarda', 'Francisca']]" ~: [("Ana",1),("Beatriz",2),("Claudia",3),("Diana",4),("Eduarda",5),("Francisca",6)] ~=? g [["Ana", "Beatriz"], ["Claudia", "Diana"], ["Eduarda", "Francisca"]]
testeG2 = "Teste g [['Ana', 'Beatriz']]" ~: [("Ana",1),("Beatriz",2)] ~=? g [["Ana", "Beatriz"]]
testeG3 = "Teste g []" ~: [] ~=? g []
testesG = test [testeG1, testeG2, testeG3]












type Posicao = (Int, Int)
data Movimento = N | S | E | O deriving (Show, Eq)

mov :: Posicao -> Movimento -> Posicao

mov (x, y) m | m == N = (x, y+1)
             | m == S = (x, y-1) 
             | m == E = (x+1, y)
             | m == O = (x-1, y)

val :: Posicao -> [Posicao] -> Int -> Int
val pt [] acc = acc
val pt (h:t) acc | pt == h   = val pt t (acc+1)
                 | otherwise = val pt t acc

h :: Posicao -> [Movimento] -> [Posicao] -> Int
h _ _ [] = 0 
h pt [] ldg = val pt ldg 0
h pt (x:xs) ldg = val pt ldg 0 + h (mov pt x) xs ldg