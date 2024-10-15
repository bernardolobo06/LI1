module Ficha4_Spec where

import Ficha4
import Test.HUnit

testsElemList = test [
    "Teste elemList 5 [4, 5, 6, 7]"   ~: 1    ~=? elemList 5 [4, 5, 6, 7],
    "Teste elemList 2 []"             ~: -1   ~=? elemList 2 [],
    "Teste elemList 2 [3..9]"         ~: -1   ~=? elemList 2 [3..9]
    ]

testsReplace = test [
    "Teste replace 1 0 [4, 5, 6, 7]"    ~: [1, 5, 6, 7]    ~=? replace 1 0 [4, 5, 6, 7],
    "Teste replace 3 5 []"              ~: []              ~=? replace 3 5 [],
    "Teste replace 7 8 [4, 5, 6, 7]"    ~: [4, 5, 6, 7]    ~=? replace 7 8 [4, 5, 6, 7]
    ]

{-
ghci -i="src" -i="tests" tests/Ficha4_Spec.hs
runTestTT tests...
-}

m1 = [[1,2,3],
      [4,5,6],
      [7,8,9]]

m2 = ["AAAA",
      "BBBB",
      "CCCC"]