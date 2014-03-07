import Mouse
import Window
import Random

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
              , vel = (0,-100)
              , rad = 15
              , col = lightRed }

defaultPlayer = { defaultPill | col <- black
                ,               pos <- (0, 0) }

type Game = { player:Pill, pills:[Pill], score:Int }

defaultGame = { player = defaultPlayer
              , pills  = [] 
              , score = 0
              }

newPill : Float -> Color -> Pill
newPill x col = { defaultPill | pos <- (x, hHeight) 
                              , col <- col }

data Event = Tick (Time, (Int, Int)) | Add Pill

stepGame: Event -> Game -> Game 
stepGame event g = 
   case event of 
       Tick (t, mouse) -> let hit pill = 
                                (vecLen <| vecSub g.player.pos pill.pos) 
                                  < g.player.rad + pill.rad
                              offscreen {pos} = snd pos > -hHeight
                              unculled = filter offscreen g.pills
                              untouched = filter (not . hit) unculled
                          in { g | player <- stepPlayer mouse g.player
                             , pills <- map (stepPill t) untouched }
       Add p           -> { g | pills <- p :: g.pills }

stepPlayer : (Int, Int) -> Pill -> Pill
stepPlayer (x,y) p = { p | pos <- (toFloat x, toFloat y) }

stepPill : Time -> Pill -> Pill
stepPill t p = { p | pos <- vecAdd p.pos (vecMulS p.vel t) }

tf : Float -> Float -> String -> Form
tf y scl str = toText str |> Text.color gray
                          |> text 
                          |> toForm
                          |> scale scl 
                          |> move (0, y)


render : (Int, Int) -> Game -> Element
render (w, h) g = 
    let formPill {rad, col, pos} = 
                     circle rad |> filled col
                                |> move pos
        txt = tf 0 2 "Simon"
        forms = txt :: (map formPill <| g.player :: g.pills)
    in color lightGray <| container w h middle 
                       <| color white
                       <| collage width height forms

delta = (fps 30)

input = (,) <~ lift inSeconds delta
             ~ sampleOn delta 
                        (lift2 relativeMouse 
                               (lift center Window.dimensions) 
                               Mouse.position)

randFloat sig = (lift (\x -> x / 100) 
                        (lift toFloat (Random.range 0 100 sig)))

rand fn sig = lift fn (randFloat sig)
randX = rand (\r -> (width * r) + -hWidth)
randCol = rand (\r -> if r < 0.1 then lightBlue else defaultPill.col)

interval = (every (second * 2))

event = merges [ lift Tick input
               , lift2 (\x col -> Add (newPill x col)) 
                         (randX interval) (randCol interval) ]

main : Signal Element
main = render <~ Window.dimensions ~ foldp stepGame defaultGame event
