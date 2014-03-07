import Mouse

relativeMouse mp = asText (fst mp - 100, snd mp - 100)

main : Signal Element
main = lift relativeMouse Mouse.position


