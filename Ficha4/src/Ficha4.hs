module Ficha4 where

--exercícios da Ficha 3
elemList :: Eq a => a -> [a] -> Integer
elemList el ls = aux el 0 ls
    where aux x p (h:t)
            | x == h = p
            | x /= h = aux x (p+1) t
          aux _ _ [] = -1

replace :: Eq a => a -> Integer -> [a] -> [a]
replace _ _ [] = []
replace el 0 (h:t) = el:t
replace el ord (h:t) = h : replace el (ord-1) t

--Ficha 4

type Matriz a = [[a]]
 
tradeFstLst :: Matriz a -> Matriz a
tradeFstLst [] = []
tradeFstLst [l] = [l]
tradeFstLst (l:m) = (last m : init m) ++ [l]