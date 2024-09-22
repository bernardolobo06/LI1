a :: [a] -> [a]
a x = [head x, last x]

b :: [String] -> [String]
b x = [head x, last x] :: [String]

c :: [String] -> String
c [] = ""
c x = head x ++ " " ++ last x

d :: [String] -> Int
d [] = 0
d x = length (last x)

e :: Char -> String -> Bool
e = elem 

f :: [a] -> [a]
f x
  | even (length x) = tail x
  | otherwise = init x

gi :: [Int] -> [Int] -> [Int]
gi x y = x ++ y

gii :: [Int] -> [Int] -> Int
gii x y = length (x ++ y)

giii :: [Int] -> [Int] -> Int
giii x y
  | length x > length y = length x - length y 
  | otherwise = length y - length x

giv :: Int -> [Int] -> String
giv x y
  | x `elem` y = "Sim"
  | otherwise = "Nao"

h :: [Int] -> [Int] -> [Int]
h x y
  | length x < length y = x ++ y 
  | otherwise = y ++ x

i :: [Int] -> [Int] -> [Int]
i x y 
  | head x < head y = x ++ y 
  | otherwise = y ++ x

j :: ([a], [a]) -> ([a], [a])
j (xs, ys) = ([head xs], ys)

k :: [String] -> String
k x = [head (head x)] ++ "." ++ last x

l :: [(Int, Int)] -> Int
l [] = 0
l x = snd (head x)

m :: [(Int, Int)] -> Int
m [] = 0
m x = uncurry (+) (last x)  -- fst (last x) + snd (last x): alterado pelo sistema

n :: (String, Int) -> (String, Int) -> String
n (x1, y1) (x2, y2)
  | y1 > y2 = x2
  | otherwise = x1

o :: (Int, Int) -> Int -> (Int, Int) 
o (x, y) lado = (x+lado, y-lado)

p :: ((Int, Int), Int) -> ((Int, Int), Int) -> Int
p ((x1, y1), l1) ((x2, y2), l2)
  | y1 < y2 = l2^2
  | y1 > y2 = l1^2
  | otherwise = if l1 > l2 then l2^2 else l1^2