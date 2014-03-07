import Mouse

relativeMouse : (Int, Int) -> (Int, Int)
relativeMouse mp = (fst mp - 100, snd mp - 100)

myText : (Int, Int) -> Element
myText mp = asText (relativeMouse mp)

main : Signal Element
main = lift myText Mouse.position


