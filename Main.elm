import Mouse
import Window

relativeMouse : (Int, Int) -> (Int, Int) -> (Int, Int)
relativeMouse (ox, oy) (x,y) = (x - ox, -(y - oy))

center : (Int, Int) -> (Int, Int)
center (w, h) = (div w 2, div h 2)

defaultPill = { pos = (0,0)
              , vel = (0,-1)
              , rad = 15
              , col = lightRed }


render (w, h) = 
    let formPill {rad, col} = circle rad |> filled col
        forms = [formPill defaultPill] 
    in color lightGray <| container w h middle 
                       <| color white
                       <| collage 400 400 forms

main : Signal Element
main = lift render Window.dimensions


