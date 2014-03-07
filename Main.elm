import Mouse
import Window

relativeMouse : (Int, Int) -> (Int, Int) -> (Int, Int)
relativeMouse (ox, oy) (x,y) = (x - ox, -(y - oy))

center : (Int, Int) -> (Int, Int)
center (w, h) = (div w 2, div h 2)

type Vec = (Float, Float)

vecAdd : Vec -> Vec -> Vec
vecAdd (x1,y1) (x2,y2) = (x1+x2, y1+y2)

type Pill = {pos:Vec, vel:Vec, rad:Float, col:Color}


defaultPill = { pos = (0,0)
              , vel = (0,-1)
              , rad = 15
              , col = lightRed }

stepPill : Time -> Pill -> Pill
stepPill t p = { p | pos <- vecAdd p.pos p.vel }

render : (Int, Int) -> Pill -> Element
render (w, h) pill = 
    let formPill {rad, col, pos} = 
                     circle rad |> filled col
                                |> move pos
        forms = [formPill pill] 
    in color lightGray <| container w h middle 
                       <| color white
                       <| collage 400 400 forms

main : Signal Element
main = render <~ Window.dimensions ~ foldp stepPill defaultPill (fps 30)


