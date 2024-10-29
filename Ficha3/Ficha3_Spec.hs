module Ficha3_Spec where

import Ficha3
import Test.HUnit

testsRemove = test [
    "Teste remove ['Ola', 'Amor', 'Comida'] 'A'" ~: ["Ola", "Comida"]         ~=? remove ["Ola", "Amor", "Comida"] 'A',
    "Teste remove ['Ola', 'Amor', 'Comida'] 'a'" ~: ["Ola", "Amor", "Comida"] ~=? remove ["Ola", "Amor", "Comida"] 'a',
    "Teste remove [] 'a'"                        ~: []                        ~=? remove [] 'a'
    ]

testsAddTuple = test [
    "Teste addTuple [(1, 2), (-1, 1), (0, 0)] 5"    ~: [(6, 2), (4, 1), (5, 0)]   ~=? addTuple [(1, 2), (-1, 1), (0, 0)] 5,
    "Teste addTuple [(1, 2), (-1, 1), (0, 0)] (-1)" ~: [(0, 2), (-2, 1), (-1, 0)] ~=? addTuple [(1, 2), (-1, 1), (0, 0)] (-1),
    "Teste addTuple [(1, 2), (-1, 1), (0, 0)] 0"    ~: [(1, 2), (-1, 1), (0, 0)]  ~=? addTuple [(1, 2), (-1, 1), (0, 0)] 0,
    "Teste addTuple [] 9"                           ~: []                         ~=? addTuple [] 9
    ]

testsMaxSndTuple = test [
    "Teste maxSndTuple [(1, 2), (-1, 1), (0, 0)]" ~: 2 ~=? maxSndTuple [(1, 2), (-1, 1), (0, 0)],
    "Teste maxSndTuple [(1, 2), (-1, 1), (0, 3)]" ~: 3 ~=? maxSndTuple [(1, 2), (-1, 1), (0, 3)]
    ]

testsAuxCoord = test [
    "Teste auxCoord (Pos 'Afonso' (0, 0)) N" ~: Pos "Afonso" (0, 1)  ~=? auxCoord (Pos "Afonso" (0, 0)) N,
    "Teste auxCoord (Pos 'Afonso' (0, 0)) S" ~: Pos "Afonso" (0, -1) ~=? auxCoord (Pos "Afonso" (0, 0)) S,
    "Teste auxCoord (Pos 'Afonso' (0, 0)) E" ~: Pos "Afonso" (1, 0)  ~=? auxCoord (Pos "Afonso" (0, 0)) E,
    "Teste auxCoord (Pos 'Afonso' (0, 0)) W" ~: Pos "Afonso" (-1, 0) ~=? auxCoord (Pos "Afonso" (0, 0)) W
    ]

testsPosicoesM = test [
    "Teste posicoesM [(Pos 'Afonso' (0, 0)), (Pos 'Bernardo' (5, 3))] N" ~: [Pos "Afonso" (0, 1), Pos "Bernardo" (5, 4)]  ~=? posicoesM [Pos "Afonso" (0, 0), Pos "Bernardo" (5, 3)] N,
    "Teste posicoesM [(Pos 'Afonso' (0, 0)), (Pos 'Bernardo' (5, 3))] S" ~: [Pos "Afonso" (0, -1), Pos "Bernardo" (5, 2)] ~=? posicoesM [Pos "Afonso" (0, 0), Pos "Bernardo" (5, 3)] S,
    "Teste posicoesM [(Pos 'Afonso' (0, 0)), (Pos 'Bernardo' (5, 3))] E" ~: [Pos "Afonso" (1, 0), Pos "Bernardo" (6, 3)]  ~=? posicoesM [Pos "Afonso" (0, 0), Pos "Bernardo" (5, 3)] E,
    "Teste posicoesM [(Pos 'Afonso' (0, 0)), (Pos 'Bernardo' (5, 3))] W" ~: [Pos "Afonso" (-1, 0), Pos "Bernardo" (4, 3)] ~=? posicoesM [Pos "Afonso" (0, 0), Pos "Bernardo" (5, 3)] W
    ]

testsPosicao = test [
    "Teste posicao (Pos 'Afonso' (0, 0)) [S, S, W]"    ~: Pos "Afonso" (-1, -2) ~=? posicao (Pos "Afonso" (0, 0)) [S, S, W],
    "Teste posicao (Pos 'Afonso' (0, 0)) [N, N, E, S]" ~: Pos "Afonso" (1, 1)   ~=? posicao (Pos "Afonso" (0, 0)) [N, N, E, S]
    ]

testsPosicoesMs = test [
    "Teste posicoesMs [(Pos 'Afonso' (0, 0)), (Pos 'Bernardo' (5, -5))] [S, S, W]"    ~: [Pos "Afonso" (-1, -2), Pos "Bernardo" (4, -7)] ~=? posicoesMs [Pos "Afonso" (0, 0), Pos "Bernardo" (5, -5)] [S, S, W],
    "Teste posicoesMs [(Pos 'Afonso' (0, 0)), (Pos 'Bernardo' (5, -5))] [N, N, E, S]" ~: [Pos "Afonso" (1, 1), Pos "Bernardo" (6, -4)]   ~=? posicoesMs [Pos "Afonso" (0, 0), Pos "Bernardo" (5, -5)] [N, N, E, S]
    ]

{-
ghci Ficha3_Spec.hs
runTestTT tests...
-}