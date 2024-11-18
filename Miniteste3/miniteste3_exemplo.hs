module Main where

import Test.HUnit
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

type Coordenadas = (Float,Float)
type Velocidade = (Float,Float)
type Tempo = Float
type Circulo = (Coordenadas, Float)
type Circulos = [Circulo]
type Estado = (Coordenadas, Velocidade, Circulos, Tempo)

estadoInicial :: Estado
estadoInicial = ((-20,100), (0.4, -0.8), [((0, 0), 15),((130, 80), 15)], 0)

desenhaCirculos :: [Circulo] -> [Picture]
desenhaCirculos [] = []
desenhaCirculos (x:xs) = desenhaCirculo x : desenhaCirculos xs
    where
        desenhaCirculo ((x, y), r) = translate x y $ color yellow $ circleSolid r

desenhaEstado :: Estado -> Picture
desenhaEstado ((x,y), (vx,vy), circ, t) = Pictures (desenhaCirculos circ ++ [translate x y figura, tempo])
    where
        figura = color c $ circleSolid 20
        c = if vy<0 then red else green
        tempo = translate 150 150 $ scale 0.1 0.1 $ Text (show $ floor t)

reageEvento :: Event -> Estado -> Estado
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) ((x,y),v,c,t)    = ((x, min (y+5) 190),v,c,t)
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) ((x,y),v,c,t)  = ((x, max (y-5) (-190)),v,c,t)
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) ((x,y),v,c,t) = ((min (x+5) 190,y),v,c,t)
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) ((x,y),v,c,t)  = ((max (x-5) (-190),y),v,c,t)
reageEvento _ s = s -- ignora qualquer outro evento

reageTempo :: Float -> Estado -> Estado
reageTempo n ((x,y),(vx,vy),c,t) =
    colide ((x+vx',y+vy'),(vx',vy'),c,t+n)
    where vy' = if y+vy<=(-190) || y+vy>=190 then -vy else vy
          vx' = if x+vx<=(-190) || x+vx>=190 then -vx else vx

colide :: Estado -> Estado
colide ((x,y), (vx,vy), circ, t) = ((x,y), (vx,vy), filter dist circ, t)
    where
        dist ((cx, cy), r) = sqrt ((x - cx)^2 + (y - cy)^2) > (r + 20)

fr:: Int
fr = 50

dm :: Display
dm = InWindow "Miniteste 3" -- título da janela
              (400,400)     -- dimensão da janela
              (200,200)     -- posição no ecran

corFundo = greyN 0.5

main :: IO ()
main = do play  dm            -- janela onde irá decorrer o jogo
                corFundo      -- cor do fundo da janela
                fr            -- frame rate
                estadoInicial -- define estado inicial do jogo
                desenhaEstado -- desenha o estado do jogo
                reageEvento   -- reage a um evento
                reageTempo    -- reage ao passar do tempo