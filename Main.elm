import Mouse
import Window

relativeMouse : (Int, Int) -> (Int, Int) -> (Int, Int)
relativeMouse (ox, oy) (x,y) = (x - ox, y - oy)

main : Signal Element
main = lift asText <| relativeMouse <~ Window.dimensions ~ Mouse.position


