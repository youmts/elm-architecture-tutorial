module Arkanoid exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (src)
import Html.Events exposing(..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Task
import String exposing (fromFloat)

main = 
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

type alias Model =
  {}
init : () -> (Model, Cmd Msg)
init _ =
  ( Model
  , send Nothing
  )

type Msg
    = Nothing

send: Msg -> Cmd Msg
send msg =
  Task.succeed msg
  |> Task.perform identity

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of
     Nothing ->
      (model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

view : Model -> Html Msg
view model =
  div
    []
    [ gameField
    ]

w : Float
w = 600

h : Float
h = 800

gameField : Html Msg
gameField =
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
        []
    ]
