import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Time exposing (..)
import Window


-- MODEL

type alias Model =
  { x : Float
  , y : Float
  , vx : Float
  , vy : Float
  , dir : Direction
  }


type Direction = Left | Right

type alias Keys = { x:Int, y:Int }


chad : Model
chad =
  { x = 0
  , y = 0
  , vx = 0
  , vy = 0
  , dir = Right
  }


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
      if chad.x < 250 then
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
          if chad.x < 250 then
            Left
          else
            chad.dir

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

    chadImage =
      rect 4 12
      |> filled skinColor

    chadSign =
      image 42 55 "./chad-sign.png"

    jasperImage =
      rect 4 12
      |> filled skinColor

    groundY = 50 - h/2

    position =
      (chad.x, chad.y + groundY)

    signImage = 
      if chad.x < 250 then
        rect 100 20
        |> filled (rgb 256 0 0)
      else
        rect 100 20
        |> filled (rgb 256 256 0)   

  in
    collage w' h'
      [ rect w h
          --           # 17 5C FE
          |> filled (rgb 23 92 254)

      , rect w 50
          |> filled (rgb 100 117 90)
          |> move (0, 24 - h/2)

      , chadImage
          |> move position

      --, chadSign
      --    |> toForm
      --    |> move (0, 104 - h/2)

      , jasperImage
          |> move (300, 50 - h/2)
          
      , signImage
          |> move (250, 300 - h/2)
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