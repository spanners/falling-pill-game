import Mouse
import Window

relativeMouse : (Int, Int) -> (Int, Int) -> (Int, Int)
relativeMouse (ox, oy) (x,y) = (x - ox, -(y - oy))

center : (Int, Int) -> (Int, Int)
center (w, h) = (div w 2, div h 2)

type Vec = (Float, Float)

vecAdd : Vec -> Vec -> Vec
vecAdd (x1,y1) (x2,y2) = (x1+x2, y1+y2)

vecMulS : Vec -> Time -> Vec
vecMulS (x, y) t = (x*t, y*t)

type Pill = {pos:Vec, vel:Vec, rad:Float, col:Color}


defaultPill = { pos = (0,0)
              , vel = (0,-30)
              , rad = 15
              , col = lightRed }

defaultPlayer = { defaultPill | col <- black }

type Game = { player:Pill, pill:Pill}

defaultGame = { player = defaultPlayer
              , pill   = defaultPill
              }

stepGame: (Time, (Int, Int)) -> Game -> Game 
stepGame (t, mouse) g = { g | player <- stepPlayer mouse g.player
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
                       <| collage 400 400 forms


input = (,) <~ lift inSeconds (fps 30)
             ~ lift2 relativeMouse (lift center Window.dimensions) Mouse.position

main : Signal Element
main = render <~ Window.dimensions ~ foldp stepGame defaultGame input 
