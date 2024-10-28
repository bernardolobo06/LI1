module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

type Estado = ((Float, Float), (Float, Float), Float) -- ((x, y), (vx, vy), time)

estadoInicial :: Estado
estadoInicial = ((-180,-180), (1,1), 0)

desenhaEstado :: Estado -> Picture
desenhaEstado ((x,y), _, t) = Pictures [translate x y poligono, translate 180 180 time]
  where poligono :: Picture
        poligono = color red $ polygon [(0,0), (10,0), (10,10), (0,10), (0,0)]
        time = scale 0.1 0.1 $ Text (show (round t))

reageEvento :: Event -> Estado -> Estado
{-
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) ((x,y),t) = ((x, min 180 y+5),t)
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) ((x,y),t) = ((x, max (-180) y-5),t)
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) ((x,y),t) = ((max (-180) x-5, y),t)
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) ((x,y),t) = ((min 180 x+5, y),t)
-}
reageEvento _ s = s -- ignora qualquer outro evento


reageTempo :: Float -> Estado -> Estado
reageTempo n ((x,y),(vx,vy),t) = ((x+vx',y+vy'),(vx',vy'),t)
  where vx' | x+vx >= 180 || x+vx <= -180 = -vx
            | otherwise = vx
        vy' | y+vy >= 180 || y+vy <= -180 = -vy
            | otherwise = vy


--((min 180 x+vx, min 180 y+vy),(vx,vy),t+n) 

fr:: Int
fr = 50

dm :: Display
dm = InWindow "Novo Jogo" -- título da janela
               (400,400) -- dimensão da janela
               (200,200) -- posição no ecra

corFundo = greyN 0.5

main :: IO ()
main = do play dm -- janela onde irá decorrer o jogo
               corFundo -- cor do fundo da janela
               fr -- frame rate
               estadoInicial -- define estado inicial do jogo
               desenhaEstado -- desenha o estado do jogo
               reageEvento -- reage a um evento
               reageTempo -- reage ao passar do tempo