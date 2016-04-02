import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Time exposing (..)
import Window


-- MODEL

type alias Model =
  { x : Float
  , vx : Float
  , dir : Direction
  }


type Direction = Left | Right

type alias Keys = { x:Int, y:Int }


chad : Model
chad =
  { x = -300
  , vx = 0
  , dir = Right
  }


bound : Float
bound = 75

-- UPDATE

update : (Float, Keys) -> Model -> Model
update (dt, keys) chad =
  chad
    |> walk keys
    |> movement dt

movement : Float -> Model -> Model
movement dt chad =
  let
    vx' = 
      if chad.x < bound then
        dt * chad.vx * 5
      else
        0
  in
    { chad |
        x = chad.x + vx'
    }


walk : Keys -> Model -> Model
walk keys chad =
  { chad |
      vx = toFloat keys.x,
      dir =
        if keys.x < 0 then
            Left

        else if keys.x > 0 then
            Right

        else
            chad.dir
  }


-- VIEW

view : (Int, Int) -> Model -> Element
view (w',h') chad =
  let
    (w,h) = (toFloat w', toFloat h')

    skinColor =
      rgb 252 162 132

    nothing =
      rect 1 1
      |> filled (rgb 23 92 254)

    chadImage =
      image 136 168 "chad.png"
      |> toForm

    chadSign =
      if chad.x < bound then
        image 42 55 "./chad-sign.png"
        |> toForm
      else
        nothing

    jasperImage =
      image 124 160 "./jasper.png"
      |> toForm



    jasperSign = 
      if chad.x < bound then
        image 79 71 "./jasper-sign.png"
        |> toForm
      else
        nothing

    signImage = 
      if chad.x < bound then
        nothing
      else
        image 1254 262 "./birthday-sign.png"
        |> toForm 

    birthdaySignPos =
      (-20, 500 - h/2)


  in
    collage w' h'
      [ rect w h
        |> filled (rgb 23 92 254)

      , chadImage
        |> move (chad.x, 80 - h/2)

      , chadSign
        |> move (chad.x + 10, 200 - h/2)

      , jasperImage
        |> move (300, 80 - h/2)

      , jasperSign 
        |> move (295, 200 - h/2)

      , signImage
        |> move birthdaySignPos
      ]


-- SIGNALS

main : Signal Element
main =
  Signal.map2 view Window.dimensions (Signal.foldp update chad input)


input : Signal (Float, Keys)
input =
  let
    delta = Signal.map (\t -> t/20) (fps 30)
  in
    Signal.sampleOn delta (Signal.map2 (,) delta Keyboard.arrows)


