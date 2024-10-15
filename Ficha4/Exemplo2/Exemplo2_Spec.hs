module Exemplo2_Spec where

import Exemplo2
import Test.HUnit

tests = test [
    "Teste mydiv 1 3"     ~: 0.33    ~=? mydiv 1 3,
    "Teste mydiv 5 0"     ~: 0       ~=? mydiv 5 0,
    "Teste mydiv1 5 0"    ~: 0       ~=? mydiv1 5 0,
    "Teste mydiv 15 3"    ~: 5       ~=? mydiv 15 3
    ]