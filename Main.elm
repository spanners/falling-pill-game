import Mouse
import Window

(width, height) = (400, 400)
(hWidth, hHeight) = (width / 2, height / 2)

relativeMouse : (Int, Int) -> (Int, Int) -> (Int, Int)
relativeMouse (ox, oy) (x,y) = (x - ox, -(y - oy))

center : (Int, Int) -> (Int, Int)
center (w, h) = (div w 2, div h 2)

type Vec = (Float, Float)

vecAdd : Vec -> Vec -> Vec
vecAdd (x1,y1) (x2,y2) = (x1+x2, y1+y2)

vecSub : Vec -> Vec -> Vec
vecSub (x1,y1) (x2,y2) = (x1-x2, y1-y2)

vecLen : Vec -> Float
vecLen (x,y) = sqrt (x * x + y * y)

vecMulS : Vec -> Time -> Vec
vecMulS (x, y) t = (x*t, y*t)

type Pill = {pos:Vec, vel:Vec, rad:Float, col:Color}


defaultPill = { pos = (0,hHeight)
              , vel = (0,-30)
              , rad = 15
              , col = lightRed }

defaultPlayer = { defaultPill | col <- black
                ,               pos <- (0, 0) }

type Game = { player:Pill, pill:Pill}

defaultGame = { player = defaultPlayer
              , pill   = defaultPill
              }

stepGame: (Time, (Int, Int)) -> Game -> Game 
stepGame (t, mouse) ({player, pill} as g) = 
   let hit = (vecLen <| vecSub player.pos pill.pos) < player.rad + pill.rad
       player' = { player | col <- if hit then lightRed else player.col }
   in { g | player <- stepPlayer mouse player'
      , pill <- stepPill t g.pill
      }

stepPlayer : (Int, Int) -> Pill -> Pill
stepPlayer (x,y) p = { p | pos <- (toFloat x, toFloat y) }

stepPill : Time -> Pill -> Pill
stepPill t p = { p | pos <- vecAdd p.pos (vecMulS p.vel t) }

render : (Int, Int) -> Game -> Element
render (w, h) game = 
    let formPill {rad, col, pos} = 
                     circle rad |> filled col
                                |> move pos
        forms = [formPill game.player, formPill game.pill] 
    in color lightGray <| container w h middle 
                       <| color white
                       <| collage width height forms

delta = (fps 30)

input = (,) <~ lift inSeconds delta
             ~ sampleOn delta (lift2 relativeMouse (lift center Window.dimensions) Mouse.position)

main : Signal Element
main = render <~ Window.dimensions ~ foldp stepGame defaultGame input 
