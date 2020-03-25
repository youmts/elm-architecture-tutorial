module Arkanoid exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (src)
import Html.Events exposing(..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Task
import String exposing (fromFloat)
import Browser.Events exposing (onAnimationFrameDelta)

main = 
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

type alias Vector = (Float, Float)

type alias Ball = 
  { position : Vector
  , velocity : Vector
  }
type alias Model =
  { ball : Ball
  }
init : () -> (Model, Cmd Msg)
init _ =
  ( Model 
    ( Ball (0, 100) (5, 5)
    )
  , send Nothing
  )

type Msg
    = Nothing
    | Tick Float

send: Msg -> Cmd Msg
send msg =
  Task.succeed msg
  |> Task.perform identity

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of
    Nothing ->
      (model, Cmd.none)
    Tick _ ->
      ( { model | ball = model.ball |> moveBall |> collisionBall }
      , Cmd.none
      )

moveBall : Ball -> Ball
moveBall old =
  { old | position = addVector old.position old.velocity}

collisionBall : Ball -> Ball
collisionBall old =
  { old |
    velocity = 
      if collisionX old.position then reverseX old.velocity
      else if collisionY old.position then reverseY old.velocity
      else old.velocity
  }

collisionX : Vector -> Bool
collisionX pos =
  let
    x = Tuple.first pos
  in
    x < 0 || x > w

collisionY : Vector -> Bool
collisionY pos =
  let
    y = Tuple.second pos
  in
    y < 0 || y > h

reverseX : Vector -> Vector
reverseX old = 
  (-(Tuple.first old), Tuple.second old)

reverseY : Vector -> Vector
reverseY old = 
  (Tuple.first old, -(Tuple.second old))

subscriptions : Model -> Sub Msg
subscriptions model =
  onAnimationFrameDelta Tick

view : Model -> Html Msg
view model =
  div
    []
    [ gameField model
    ]

w : Float
w = 600

h : Float
h = 800

gameField : Model -> Html Msg

gameField model =
  svg
    [ width (fromFloat w)
    , height (fromFloat h)
    , viewBox "0 0 600 800" ]
    [
      rect
        [ x "0"
        , y "0"
        , width (fromFloat w)
        , height (fromFloat h)
        , fill "black"
        ]
        [ 
        ]
    , svgBall model.ball.position
    ]

svgBall : Vector -> Html Msg
svgBall (x, y) =
  circle
    [ cx (fromFloat x)
    , cy (fromFloat y)
    , r "10"
    , fill "red"
    ]
    []

addVector : Vector -> Vector -> Vector
addVector a b =
   (Tuple.first a + Tuple.first b, Tuple.second a + Tuple.second b)