module MyRandom exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (src)
import Html.Events exposing(..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import List exposing (singleton, append)
import Random
import Task

main = 
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

type Die
  = F1
  | F2
  | F3
  | F4
  | F5
  | F6

type alias Model = 
  { dieFaceOne : Die
  , dieFaceTwo : Die
  }
  
-- https://medium.com/elm-shorts/how-to-turn-a-msg-into-a-cmd-msg-in-elm-5dd095175d84
init : () -> (Model, Cmd Msg)
init _ =
  ( Model F1 F1
  , send Roll
  )

type Msg
    = Roll
    | NewFace Model

die : Random.Generator Die
die = 
  Random.weighted
    (10, F1)
    [ (10, F2)
    , (10, F3)
    , (10, F4)
    , (10, F5)
    , (10, F6)
    ]

roll : Random.Generator Model
roll =
  Random.map2 Model die die

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Roll ->
      ( model
      , Random.generate NewFace roll
      )

    NewFace newFace ->
      ( newFace
      , Cmd.none
      )

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

view : Model -> Html Msg
view model =
  div []
    [ dieView model.dieFaceOne
    , dieView model.dieFaceTwo
    , button [ onClick Roll ] [ Html.text "Roll" ]
    ]

dieView : Die -> Html Msg
dieView dieFace =
  case dieFace of
    F1 -> dieSvg 
      [ circle
        [ cx "60"
        , cy "60"
        , r "15"
        , fill "red"
        ]
        []
      ]
    F2 -> img [ src "https://d3cpdjqy5ztwui.cloudfront.net/illust_data/000480/480440/480440m.jpg" ] []
    F3 -> img [ src "https://d3cpdjqy5ztwui.cloudfront.net/illust_data/000480/480441/480441m.jpg" ] []
    F4 -> img [ src "https://d3cpdjqy5ztwui.cloudfront.net/illust_data/000480/480452/480452m.jpg" ] []
    F5 -> img [ src "https://d3cpdjqy5ztwui.cloudfront.net/illust_data/000480/480455/480455m.jpg" ] []
    F6 -> img [ src "https://d3cpdjqy5ztwui.cloudfront.net/illust_data/000480/480457/480457m.jpg" ] []

dieSvg: List (Svg Msg) -> Svg Msg
dieSvg list =
  svg
    [ width "120"
    , height "120"
    , viewBox "0 0 120 120"
    ]
    (append (singleton dieRect) list)

dieRect: Svg Msg
dieRect =
  rect 
    [ x "10"
    , y "10"
    , width "100"
    , height "100"
    , rx "10"
    , ry "10"
    , fill "white"
    , stroke "black"
    ]
    []

send: Msg -> Cmd Msg
send msg =
  Task.succeed msg
  |> Task.perform identity
