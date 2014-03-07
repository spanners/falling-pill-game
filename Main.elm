import Mouse
import Window

relativeMouse : (Int, Int) -> (Int, Int) -> (Int, Int)
relativeMouse (ox, oy) (x,y) = (x - ox, -(y - oy))

center : (Int, Int) -> (Int, Int)
center (w, h) = (div w 2, div h 2)

render (x, y) = let forms = [circle 15 |> filled lightBlue |> move (toFloat x, toFloat y)]
                in color gray <| collage 400 400 forms

main : Signal Element
main = lift render <| relativeMouse (200, 200) <~ Mouse.position


