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

type alias Block = 
  {
    position : Vector
  , width : Float
  , height : Float
  }

type alias Model =
  { ball : Ball
  , blocks : List Block
  }

fieldWidth : Float
fieldWidth = 400

fieldHeight : Float
fieldHeight = 600

blockNumX : Float
blockNumX = 10

blockNumY : Float
blockNumY = 30

blockWidth : Float
blockWidth = fieldWidth / blockNumX

blockHeight : Float
blockHeight = fieldHeight / blockNumY

init : () -> (Model, Cmd Msg)
init _ =
  ( Model 
      (Ball (Vector 50 400) (Vector 3 3) 5)
      (List.range 3 7 |> List.map toFloat |> List.concatMap initBlockRow)
      
  , send Nothing
  )

initBlockRow : Float -> List Block
initBlockRow  indexY =
  List.range 0 9 |> List.map toFloat |> List.map (initBlock (blockHeight * indexY))

initBlock : Float -> Float -> Block
initBlock y indexX =
  Block (Vector (blockWidth * indexX) y) blockWidth blockHeight 

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
      let
        (blocks, ball) = model.ball |> moveBall |> collisionBall model.blocks
      in
        ( { model | ball = ball, blocks = blocks }
        , Cmd.none
        )

moveBall : Ball -> Ball
moveBall old =
  { old | position = addVector old.position old.velocity}

collisionBall : List Block -> Ball -> (List Block, Ball)
collisionBall oldBlocks oldBall =
  -- TODO XもYもぶつかるケースを考慮する
  let
    newVelocity = 
      if collisionX oldBall then reverseX oldBall.velocity
      else if collisionY oldBall then reverseY oldBall.velocity
      else oldBall.velocity
  in
    ( oldBlocks
    , { oldBall |
      velocity = newVelocity
      }
    )

collisionX : Ball -> Bool
collisionX ball =
  let
    x = ball.position.x
  in
    x < ball.radius || x > fieldWidth - ball.radius

collisionY : Ball -> Bool
collisionY ball =
  let
    y = ball.position.y
  in
    y < ball.radius || y > fieldHeight - ball.radius

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

gameField : Model -> Html Msg

gameField model =
  svg
    [ width (fromFloat fieldWidth)
    , height (fromFloat fieldHeight)
    , viewBox ("0 0 " ++ fromFloat fieldWidth ++ " " ++ fromFloat fieldHeight)
    ]
    ( List.append [
        rect
          [ x "0"
          , y "0"
          , width (fromFloat fieldWidth)
          , height (fromFloat fieldHeight)
          , fill "black"
          ]
          [ 
          ]
      , svgBall model.ball
      ] (model.blocks |> List.map svgBlock)
    )

svgBall : Ball -> Html Msg
svgBall ball =
  circle
    [ cx (fromFloat ball.position.x)
    , cy (fromFloat ball.position.y)
    , r (fromFloat ball.radius)
    , fill "red"
    ]
    []

svgBlock : Block -> Svg Msg
svgBlock block =
  rect
    [ x (fromFloat block.position.x)
    , y (fromFloat block.position.y)
    , width (fromFloat block.width)
    , height (fromFloat block.height)
    , fill "white"
    , stroke "black"
    ]
    []

addVector : Vector -> Vector -> Vector
addVector a b =
  Vector (a.x + b.x) (a.y + b.y)
