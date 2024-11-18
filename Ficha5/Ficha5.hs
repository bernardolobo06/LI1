module Main where


import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game


type Estado = ((Float, Float), (Float, Float), Float) -- ((x, y), (vx, vy), time)

estadoInicial :: Estado
estadoInicial = ((0,0), (1,1), 0)


desenhaEstado :: Estado -> Picture
desenhaEstado ((x,y), _, t) = Pictures [translate x y poligono, translate 180 180 time]
  where poligono :: Picture
        poligono = color red $ polygon [(0,0), (10,0), (10,10), (0,10), (0,0)]
        time = scale 0.1 0.1 $ Text (show (round t))


reageEvento :: Event -> Estado -> Estado
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) ((x,y),(vx,vy), t) = ((x, min 180 y+5),(vx,vy),t)
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) ((x,y),(vx,vy), t) = ((x, max (-190) y-5),(vx,vy),t)
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) ((x,y),(vx,vy), t) = ((max (-190) x-5, y),(vx,vy),t)
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) ((x,y),(vx,vy), t) = ((min 180 x+5, y),(vx,vy),t)
reageEvento _ s = s -- ignora qualquer outro evento


reageTempo :: Float -> Estado -> Estado
reageTempo n ((x,y),(vx,vy),t) = ((x,y),(vx,vy),t+n)


fr:: Int
fr = 30


dm :: Display
dm = InWindow "Novo Jogo" -- título da janela
               (400,400)  -- dimensão da janela
               (200,200)  -- posição no ecra


corFundo :: Color
corFundo = greyN 0.5


main :: IO ()
main = do play dm             -- janela onde irá decorrer o jogo
               corFundo       -- cor do fundo da janela
               fr             -- frame rate
               estadoInicial  -- define estado inicial do jogo
               desenhaEstado  -- desenha o estado do jogo
               reageEvento    -- reage a um evento
               reageTempo     -- reage ao passar do tempo