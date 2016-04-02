import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Time exposing (..)
import Window


-- MODEL

type alias Model =
  { x  : Float
  , vx : Float
  }

type alias Keys = { x:Int, y:Int }


chad : Model
chad =
  { x   = -400
  , vx  = 0
  }


bound : Float
bound = 75


-- UTIL

url : String
url = "https://raw.githubusercontent.com/Chadtech/jasper-birthday/master/"

sourcer : String -> String
sourcer i =
  url ++ "/" ++ i


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
      vx = toFloat keys.x
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
      |> bluer

    bluer = 
      filled 
      <| (rgb 23 92 254)

    chadImage =
      "chad.png"
      |> sourcer
      |> image 136 168
      |> toForm

    chadSign =
      if chad.x < bound then
        "chad-sign.png"
        |> sourcer
        |> image 42 55
        |> toForm
      else
        nothing

    jasperImage =
      "jasper.png"
      |> sourcer
      |> image 124 160
      |> toForm

    jasperSign = 
      if chad.x < bound then
        "jasper-sign.png"
        |> sourcer
        |> image 79 71
        |> toForm
      else
        nothing

    signImage = 
      if chad.x < bound then
        nothing
      else
        "birthday-sign.png"
        |> sourcer
        |> image 1254 262
        |> toForm 

  in
    collage w' h'
      [ rect w h
        |> bluer

      , chadImage
        |> move (chad.x, 80 - h/2)

      , chadSign
        |> move (chad.x + 10, 200 - h/2)

      , jasperImage
        |> move (300, 80 - h/2)

      , jasperSign 
        |> move (295, 200 - h/2)

      , signImage
        |> move (-20, 500 - h/2)
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


