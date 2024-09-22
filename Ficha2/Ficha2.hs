data Movimento = Norte | Sul | Este | Oeste deriving Show

type Ponto = (Double, Double)

move :: Ponto -> Movimento -> Ponto  -- Exercicio 1
move (x, y) Norte = (x, y+1)
move (x, y) Sul = (x, y-1)
move (x, y) Este = (x+1, y)
move (x, y) Oeste = (x-1, y)

dist :: Ponto -> Ponto -> Double
dist (x1, y1) (x2, y2) = sqrt ((x1-x2)^2 + (y1-y2)^2)

pontoSul :: Ponto -> Ponto -> Ponto
pontoSul (x1, y1) (x2, y2)
    | y1 > y2 = (x2, y2)
    | otherwise = (x1, y1)

moveJanInf :: Ponto -> Movimento -> Double -> Ponto  -- Exercicio 2
moveJanInf (x, y) Norte jan = (x, min jan y+1)
moveJanInf (x, y) Sul jan = (x, max 0 y-1)
moveJanInf (x, y) Este jan = (min jan x+1, y)
moveJanInf (x, y) Oeste jan = (max jan x-1, y)

pontoJanSup :: Ponto -> Double -> Ponto  -- Exercicio 3
pontoJanSup (x, y) jan = (x, jan-y)

pontoJanCt :: Ponto -> Double -> Ponto  -- Exercicio 4
pontoJanCt (x, y) jan = (x-(jan/2), y-(jan/2))

velHor :: Ponto -> Double -> Double -> Ponto  -- Exercicio 5
velHor (x, y) vel t = (x + (vel*t), y)

velVert :: Ponto -> Double -> Double -> Ponto  -- Exercicio 6
velVert (x, y) vel t = (x, y + (vel*t))

type Velocidade = (Double, Double)

vel :: Ponto -> Velocidade -> Double -> Ponto  -- Exercicio 7
vel (x, y) (vx, vy) t = (x + (vx*t), y + (vy*t))

data Figura = Circulo Ponto Double | Rectangulo Ponto Ponto | Quadrado Ponto Double deriving (Show,Eq)

pontoFig :: Ponto -> Figura -> Bool  -- Exercicio 8
pontoFig (x, y) (Circulo (a, b) r) = (x-a)^2 + (y-b)^2 <= r^2
pontoFig (x, y) (Rectangulo (x1, y1) (x2, y2)) = (x >= min x1 x2) && (x <= max x1 x2) && (y >= min y1 y2) && (y <= max y1 y2)
pontoFig (x, y) (Quadrado (a, b) l) = (x >= a) && (x <= a+l) && (y >= b-l) && (y <= b)

menorQuadrado :: Figura -> Figura
menorQuadrado (Circulo (a, b) r) = Quadrado (a-r, b+r) (r*2)
menorQuadrado (Rectangulo (x1, y1) (x2, y2)) = Quadrado (min x1 x2, max y1 y2) (abs (x2-x1))
menorQuadrado (Quadrado (a, b) l) = Quadrado (a, b) l

maiorCirculo :: Figura -> Figura
maiorCirculo (Circulo (a, b) r) = Circulo (a, b) r
maiorCirculo (Rectangulo (x1, y1) (x2, y2))
    | max x1 x2 - min x1 x2 > max y1 y2 - min y1 y2 = Circulo (min x1 x2 + (max y1 y2 - min y1 y2)/2 , max y1 y2 - (max y1 y2 - min y1 y2)/2) ((max y1 y2 - min y1 y2)/2)  -- comp > alt
    | otherwise = Circulo (min x1 x2 + (max x1 x2 - min x1 x2)/2 , max y1 y2 - (max x1 x2 - min x1 x2)/2) ((max x1 x2 - min x1 x2)/2) -- comp <= alt
maiorCirculo (Quadrado (a, b) l) = Circulo (a+l/2, b-l/2) (l/2)

contida :: Figura -> Figura -> Bool
contida (Circulo (a, b) r) (Rectangulo (x1, y1) (x2, y2)) = False
contida (Circulo (a, b) r) (Quadrado (x, y) l) = False
contida (Rectangulo (x1, y1) (x2, y2)) (Quadrado (a, b) l) = False

contida (Circulo (a, b) r1) (Circulo (c, d) r2) = (a-r1 > c-r2) && (a+r1 < c+r2) && (b-r1 > d-r2) && (b+r1 < d+r2)
contida (Rectangulo (x1, y1) (x2, y2)) (Rectangulo (x3, y3) (x4, y4)) = (min x1 x2 > min x3 x4) && (max x1 x2 < max x3 x4) && (min y1 y2 > min y3 y4) && (max y1 y2 < max y3 y4)
contida (Quadrado (x1, y1) l1) (Quadrado (x2, y2) l2) = (x1 > x2) && (x1+l1 < x2+l2) && (y1-l1 > y2-l2) && (y1 < y2)

contidaDifer :: Figura -> Figura -> Bool
contidaDifer (Circulo (a, b) r) (Rectangulo (x1, y1) (x2, y2)) = (a-r > min x1 x2) && (a+r < max x1 x2) && (b-r > min y1 y2) && (b+r < max y1 y2)
contidaDifer (Rectangulo (x1, y1) (x2, y2)) (Circulo (a, b) r) = (max x1 x2-a)^2 + (max y1 y2-b)^2 <= r^2 && (min x1 x2-a)^2 + (min y1 y2-b)^2 <= r^2
contidaDifer (Circulo (a, b) r) (Quadrado (x, y) l) = (a-r > x) && (a+r < x+l) && (b-r > y-l) && (b+r < y)
-- contidaDifer (Quadrado (x, y) l) (Circulo (a, b) r) = 
-- contidaDifer (Rectangulo (x1, y1) (x2, y2)) (Quadrado (a, b) l) = 
-- contidaDifer (Quadrado (a, b) l) (Rectangulo (x1, y1) (x2, y2)) =