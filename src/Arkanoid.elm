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
import Json.Decode as Decode

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
  , bar : Block
  , leftKeyDown : Bool
  , rightKeyDown : Bool
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
      (Block (Vector 150 500) 100 20)
      False
      False
      
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
    | KeyDown KeyValue
    | KeyUp KeyValue

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
        (blocks, ball) = model.ball |> moveBall |> collisionBall model.bar model.blocks
        bar = moveBar model model.bar
      in
        ( { model | ball = ball, blocks = blocks, bar = bar }
        , Cmd.none
        )
    KeyDown keyValue ->
      -- MEMO : 何故か 'd' が捕捉できない
      case keyValue of
         Character 'a' -> ( { model | leftKeyDown = True}, Cmd.none )
         Character 's' -> ( { model | rightKeyDown = True}, Cmd.none )
         _ -> ( model, Cmd.none )
    KeyUp keyValue ->
      case keyValue of
         Character 'a' -> ( { model | leftKeyDown = False}, Cmd.none )
         Character 's' -> ( { model | rightKeyDown = False}, Cmd.none )
         _ -> ( model, Cmd.none )
      

moveBall : Ball -> Ball
moveBall old =
  { old | position = addVector old.position old.velocity}

moveBar : Model -> Block -> Block
moveBar model old =
  let 
    move = Vector
        (
          (
            (if model.rightKeyDown && old.position.x + old.width < fieldWidth then 1 else 0)
          + (if model.leftKeyDown && old.position.x > 0 then -1 else 0)
          ) * 5
        ) 0
  in
    { old | position = addVector old.position move}


type ReflectDirection
  = Keep
  | KeepForce
  | Change Float

type alias CollisionResult =
  { collision : Bool
  , x : ReflectDirection
  , y : ReflectDirection
  }

mergeReflect : ReflectDirection -> ReflectDirection -> ReflectDirection
mergeReflect base new =
  case base of
    Keep -> new
    KeepForce -> KeepForce
    Change baseValue ->
      case new of
        Keep -> Change baseValue
        KeepForce -> KeepForce
        Change newValue -> if baseValue * newValue < 0 then KeepForce else Change baseValue

changeSign : Float -> Float -> Float
changeSign sign value = sign * sqrt (value * value)

collisionBall : Block -> List Block -> Ball -> (List Block, Ball)
collisionBall bar oldBlocks oldBall =
  let
    blockCollisions = List.map (collisionBallBlock oldBall) (bar :: oldBlocks)
    reflectX = List.foldl mergeReflect (collisionWallX oldBall) (List.map (\n -> n.x) blockCollisions)
    reflectY = List.foldl mergeReflect (collisionWallY oldBall) (List.map (\n -> n.y) blockCollisions)

  in
    -- TODO: 二重に当たり判定している、上の bxys をつかってfilterするようにする
    ( List.filter (\block -> not (collisionBallBlock oldBall block).collision) oldBlocks
    , { oldBall |
      velocity = Vector 
        ( case reflectX of
          Change v -> changeSign v oldBall.velocity.x
          _ -> oldBall.velocity.x
        )
        ( case reflectY of
          Change v -> changeSign v oldBall.velocity.y
          _ -> oldBall.velocity.y
        )
      }
    )

intersect : Float -> Float -> Float -> Float -> Bool
intersect s1 e1 s2 e2 = s1 < e2 && s2 < e1

distance : Vector -> Vector -> Float
distance a b = sqrt ((a.x - b.x) ^ 2 + (a.y - b.y) ^ 2)

collisionBallBlock : Ball -> Block -> CollisionResult
collisionBallBlock ball block =
  let
    intersectBlockX = intersect block.position.x (block.position.x + block.width)
    centerBlockX = block.position.x + block.width / 2
    intersectBlockY = intersect block.position.y (block.position.y + block.height)
    centerBlockY = block.position.y + block.height / 2
  in
    if intersectBlockX ball.position.x ball.position.x then
      if intersectBlockY (ball.position.y - ball.radius) (ball.position.y + ball.radius) then
        CollisionResult True Keep (Change (if ball.position.y < centerBlockY then -1 else 1))
      else
        CollisionResult False Keep Keep
    else if intersectBlockY ball.position.y ball.position.y then
      if intersectBlockX (ball.position.x - ball.radius) (ball.position.x + ball.radius) then
        CollisionResult True (Change (if ball.position.x < centerBlockX then -1 else 1)) Keep
      else
        CollisionResult False Keep Keep
    else
      -- TODO: 斜めからあたったとき
      CollisionResult False Keep Keep

collisionWallX : Ball -> ReflectDirection
collisionWallX ball =
  let
    x = ball.position.x
  in
    if x < ball.radius then Change 1
    else if x > fieldWidth - ball.radius then Change -1
    else Keep

collisionWallY : Ball -> ReflectDirection
collisionWallY ball =
  let
    y = ball.position.y
  in
    if y < ball.radius then Change 1
    else if y > fieldHeight - ball.radius then Change -1
    else Keep

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.batch
    [ onAnimationFrameDelta Tick
    , Browser.Events.onKeyDown (Decode.map KeyDown keyDecoder) 
    , Browser.Events.onKeyUp (Decode.map KeyUp keyDecoder) 
    ]

-- https://github.com/lenards/elm-example-key-decoding

type KeyValue
    = Character Char
    | Control String

keyDecoder : Decode.Decoder KeyValue
keyDecoder =
  Decode.map toKeyValue (Decode.field "key" Decode.string)

toKeyValue : String -> KeyValue
toKeyValue string =
  case String.uncons string of
      Just ( char, "" ) ->
          Character char

      _ ->
          Control string

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
      , svgBlock model.bar
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
