import Mouse

relativeMouse : (Int, Int) -> (Int, Int)
relativeMouse (x,y) = (x - 100, y - 100)

myText : (Int, Int) -> Element
myText mp = asText <| relativeMouse mp

main : Signal Element
main = lift myText Mouse.position


