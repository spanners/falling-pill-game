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

type Game = { player:Pill, pills:[Pill] }

defaultGame = { player = defaultPlayer
              , pills  = [] }

data Event = Tick (Time, (Int, Int)) | Add Pill

stepGame: Event -> Game -> Game 
stepGame event ({player, pills} as g) = 
   case event of 
       Tick (t, mouse) -> let hit pill = (vecLen <| vecSub player.pos pill.pos) < player.rad + pill.rad
                              unculled = filter (\{pos} -> snd pos > -hHeight) pills
                              untouched = filter (not . hit) unculled
                          in { g | player <- stepPlayer mouse player
                             , pills <- map (stepPill t) untouched }
       Add p           -> { g | pills <- p :: g.pills }

stepPlayer : (Int, Int) -> Pill -> Pill
stepPlayer (x,y) p = { p | pos <- (toFloat x, toFloat y) }

stepPill : Time -> Pill -> Pill
stepPill t p = { p | pos <- vecAdd p.pos (vecMulS p.vel t) }

render : (Int, Int) -> Game -> Element
render (w, h) game = 
    let formPill {rad, col, pos} = 
                     circle rad |> filled col
                                |> move pos
        forms = formPill game.player :: map formPill game.pills 
    in color lightGray <| container w h middle 
                       <| color white
                       <| collage width height forms

delta = (fps 30)

input = (,) <~ lift inSeconds delta
             ~ sampleOn delta (lift2 relativeMouse (lift center Window.dimensions) Mouse.position)

event = merges [ lift Tick input
               , lift (Add . (\_ -> defaultPill)) (every (second * 3)) ]

main : Signal Element
main = render <~ Window.dimensions ~ foldp stepGame defaultGame event
