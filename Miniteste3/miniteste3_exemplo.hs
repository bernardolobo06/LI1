{-|
Module      : Miniteste 3
Description : Módulo Haskell que contém a resolução do Miniteste 3.
Copyright   : (c) Marcos Bernardo da Silva Lobo <a110959@alunos.uminho.pt>, 2024
Maintainer  : a110959@alunos.uminho.pt
Stability   : Experimental
Portability : POSIX (Portable Operating System Interface)

Este módulo Haskell contém a resolução do Miniteste 3.
-}

module Miniteste3 where

import Test.HUnit
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Foreign (toBool)

-- | __Coordenadas__ = (abcissa, ordenada)
type Coordenadas = (Float, Float)
-- | __Velocidade__ = (velocidade x, velocidade y)      
type Velocidade = (Float, Float)   
-- | __Tempo__ = segundos
type Tempo = Float                     
-- | __Circulos__ = [(Coordenadas, raio)]
type Circulos = [(Coordenadas, Float)]
-- | Representa o Estado do objeto jogável.
type Estado = (Coordenadas, Velocidade, Circulos, Tempo)

-- | A função 'estadoInicial' define o 'Estado' inicial de todos os objetos do Jogo.
estadoInicial :: Estado
estadoInicial = ((-20,100), (0.4, -0.8), [((0, 0), 15),((130, 80), 15)], 0)

{-| A função 'desenhaCirculos' é uma função auxiliar que escreve todas as figuras circulares definidas numa lista 'Circulos'.

==  __Exemplos de utilização:__
>>> desenhaCirculos [((0, 0), 15),((130, 80), 15)]
[translate 0.0 0.0 $ yellow circleSolid 15.0, translate 130.0 80.0 $ yellow circleSolid 15.0]

== __Propriedades:__
prop> desenhaCirculos [] = []
-}

desenhaCirculos :: Circulos -> [Picture]
desenhaCirculos [] = []
desenhaCirculos (x:xs) = desenhaCirculo x : desenhaCirculos xs
    where
        desenhaCirculo ((x, y), r) = translate x y $ color yellow $ circleSolid r

{-| A função 'desenhaEstado' desenha o 'Estado' atual de Jogo:

* Objeto jogável (cuja cor depende da velocidade y);
* 'Circulos' (recorrendo à função auxiliar 'desenhaCirculos');
* Tempo (arredondado, no canto superior direito).
-}

desenhaEstado :: Estado -> Picture
desenhaEstado ((x,y), (vx,vy), circ, t) = Pictures (desenhaCirculos circ ++ [translate x y figura, tempo])
    where
        figura = color c $ circleSolid 20
        c = if vy<0 then red else green
        tempo = translate 150 150 $ scale 0.1 0.1 $ Text (show $ floor t)

{-| A função 'reageEvento' reage aos Eventos que ocorram, nomeadamente às teclas KeyUp, KeyDown, KeyRight e KeyLeft.

Esta função faz com que o objeto jogável se mova, unicamente, dentro da janela.

==  __Exemplos de utilização:__
>>> reageEvento (EventKey (SpecialKey KeyUp) Down _ _) Estado
((x, min (y+5) 190),_,_,_)

== __Propriedades:__
Caso o Evento registado não seja nenhum dos referidos, nada acontece.

prop> reageEvento _ Estado = Estado
-}

reageEvento :: Event -> Estado -> Estado
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) ((x,y),v,c,t)    = ((x, min (y+5) 190),v,c,t)
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) ((x,y),v,c,t)  = ((x, max (y-5) (-190)),v,c,t)
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) ((x,y),v,c,t) = ((min (x+5) 190,y),v,c,t)
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) ((x,y),v,c,t)  = ((max (x-5) (-190),y),v,c,t)
reageEvento _ s = s -- ignora qualquer outro evento

{-| A função 'reageTempo' reage à passagem do Tempo.

Esta função faz com que o objeto jogável se mova automaticamente dentro da janela, e verifica se o mesmo colide com algum objeto de 'Circulos' (recorrendo à função auxilair 'colide').
-}

reageTempo :: Float -> Estado -> Estado
reageTempo n ((x,y),(vx,vy),c,t) =
    colide ((x+vx',y+vy'),(vx',vy'),c,t+n)
    where vy' = if y+vy<=(-190) || y+vy>=190 then -vy else vy
          vx' = if x+vx<=(-190) || x+vx>=190 then -vx else vx

{-| A função 'colide' é uma função auxiliar que verifica se o objeto jogável colide com alguma das figuras circulares definidas numa lista 'Circulos' e a remove se for verdade.

==  __Exemplos de utilização:__
>>> desenhaCirculos ((0,35), v, [((0, 0), 15),((130, 80), 15)], t)
((0,35), v, [((130, 80), 15)], t)
-}

colide :: Estado -> Estado
colide ((x,y), (vx,vy), circ, t) = ((x,y), (vx,vy), filter dist circ, t)
    where
        dist ((cx, cy), r) = sqrt ((x - cx)^2 + (y - cy)^2) > (r + 20)

-- | É uma função que define a /__frame rate__/, ou seja, a quantidade de quadros, que serão desenhados por segundo.
fr:: Int
fr = 50

-- | É uma função que desenha a __Janela de Jogo__ e, portanto, o seu nome, tamanho e posição no ecrã.
dm :: Display
dm = InWindow "Miniteste 3" -- título da janela
              (400,400)     -- dimensão da janela
              (200,200)     -- posição no ecran

-- | É uma função constante que define a __cor do fundo__ de 'dm'.
corFundo :: Color
corFundo = greyN 0.5

{-| A função 'main' é responsável por iniciar o programa e executar as ações principais.

Esta função configura a janela, lida com entrada/saída de dados, e chama outras funções para realizar as tarefas do programa.
-}
main :: IO ()
main = do play  dm            -- janela onde irá decorrer o jogo
                corFundo      -- cor do fundo da janela
                fr            -- frame rate
                estadoInicial -- define estado inicial do jogo
                desenhaEstado -- desenha o estado do jogo
                reageEvento   -- reage a um evento
                reageTempo    -- reage ao passar do tempo


-- HUnit Test --

estadoInicialValido :: Estado -> Bool
estadoInicialValido ((x,y), v, circ, t) = x <= 190 && x >= (-190) && y <= 190 && y >= (-190) && circValido circ
    where circValido :: Circulos -> Bool
          circValido [] = True
          circValido (((cx,cy), r):xs) = cx <= 190 && cx >= (-190) && cy <= 190 && cy >= (-190) && circValido xs
    
testEstadoInicial = test ["Teste estadoInicialValido estadoInicial" ~: True ~=? estadoInicialValido estadoInicial]