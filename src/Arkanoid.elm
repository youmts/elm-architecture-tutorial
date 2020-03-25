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

type alias Vector = 
  { x : Float
  , y : Float
  }

type alias Ball = 
  { position : Vector
  , velocity : Vector
  , radius : Float
  }
type alias Model =
  { ball : Ball
  }
init : () -> (Model, Cmd Msg)
init _ =
  ( Model 
    ( Ball (Vector 50 100) (Vector 5 5) 10
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
  -- TODO XもYもぶつかるケースを考慮する
  { old |
    velocity = 
      if collisionX old then reverseX old.velocity
      else if collisionY old then reverseY old.velocity
      else old.velocity
  }

collisionX : Ball -> Bool
collisionX ball =
  let
    x = ball.position.x
  in
    x < ball.radius || x > w - ball.radius

collisionY : Ball -> Bool
collisionY ball =
  let
    y = ball.position.y
  in
    y < ball.radius || y > h - ball.radius

reverseX : Vector -> Vector
reverseX old = 
  Vector -old.x old.y

reverseY : Vector -> Vector
reverseY old = 
  Vector old.x -old.y

subscriptions : Model -> Sub Msg
subscriptions _ =
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
    , svgBall model.ball
    ]

svgBall : Ball -> Html Msg
svgBall ball =
  circle
    [ cx (fromFloat ball.position.x)
    , cy (fromFloat ball.position.y)
    , r (fromFloat ball.radius)
    , fill "red"
    ]
    []

addVector : Vector -> Vector -> Vector
addVector a b =
  Vector (a.x + b.x) (a.y + b.y)
