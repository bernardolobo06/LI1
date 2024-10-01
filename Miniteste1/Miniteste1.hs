type Horas = (Int, Int)

minutos :: Horas -> Int
minutos (h, m) = h*60 + m

f :: [Horas] -> Horas
f horas
    | minutos (last horas) > minutos (head horas) = (div (minutos (last horas) - minutos (head horas)) 60, mod (minutos (last horas) - minutos (head horas)) 60)
    | otherwise = (abs (div (minutos (head horas) - minutos (last horas) - 24*60) 60), abs (mod (minutos (head horas) - minutos (last horas) - 24*60) 60))

type Ponto = (Double, Double)

data Figura = Circulo Ponto Double | Rectangulo Ponto Ponto | Quadrado Ponto Double deriving Show

g :: Figura -> Figura
g (Circulo (x0, y0) r) = Circulo (x0, y0) (r/2)
g (Quadrado (x0, y0) l) = Quadrado (x0, y0) (l/2)
g (Rectangulo (x0, y0) (x1, y1)) = Rectangulo (x0, y0) (x1 - (abs (x0-x1) / 2), y1 + (abs (y0-y1) / 2))