import Mouse

relativeMouse : (Int, Int) -> (Int, Int)
relativeMouse (x,y) = (x - 100, y - 100)

myText : (Int, Int) -> Element
myText = asText . relativeMouse

main : Signal Element
main = lift myText Mouse.position


