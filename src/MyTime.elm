module MyTime exposing (main)

import Browser
import Html exposing (..)
import Task
import Time 
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)
import Svg exposing (..)
import Svg.Attributes exposing (..)

main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type alias Model =
  { zone : Time.Zone
  , time : Time.Posix
  , active : Bool
  }

init : () -> (Model, Cmd Msg)
init _ =
  ( Model Time.utc (Time.millisToPosix 0) True
  , Task.perform AdjustTimeZone Time.here
  )

type Msg
  = Tick Time.Posix
  | AdjustTimeZone Time.Zone
  | TogglePause

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
      ( { model | time = newTime }
      , Cmd.none
      )

    AdjustTimeZone newZone ->
      ( { model | zone = newZone }
      , Cmd.none
      )

    TogglePause ->
      ( { model | active = not model.active }
      , Cmd.none
      )

subscriptions : Model -> Sub Msg
subscriptions model =
  if model.active then
    Time.every 1000 Tick
  else
    Sub.none

view : Model -> Html Msg
view model =
  let
    hour   = String.fromInt (Time.toHour   model.zone model.time)
    minute = String.fromInt (Time.toMinute model.zone model.time)
    second = String.fromInt (Time.toSecond model.zone model.time)
  in
  Html.div[]
  [
    Html.h1
      [ Html.Attributes.style "display" "inline-block"
      , Html.Attributes.style "border" "solid 2px #aaaaaa"
      , Html.Attributes.style "border-radius" "10px"
      ]
      [ Html.text (hour ++ ":" ++ minute ++ ":" ++ second)
      ]
    , br [] []
    , svg
      [
        width "240"
      , height "240"
      , viewBox "0 0 240 240"
      ]
      [ circle
        [ cx "120"
        , cy "120"
        , r "100"
        , fill "white"
        , stroke "black"
        , strokeWidth "2"
        ]
        [ 
        ]
      , clockHand (toFloat (Time.toHour model.zone model.time) / 12) "Black" "5"
      , clockHand (toFloat (Time.toMinute model.zone model.time) / 60) "Black" "2"
      , clockHand (toFloat (Time.toSecond model.zone model.time) / 60) "Red" "2"
      ]
    , button [ onClick TogglePause ] [ Html.text "toggle pause" ]
  ]

clockHand: Float -> String -> String -> Svg Msg
clockHand rate strokeValue strokeWidthValue =
  line
    [ x1 "120"
    , y1 "120"
    , x2 (String.fromFloat (cos (rateToPi rate) * 100 + 120))
    , y2 (String.fromFloat (sin (rateToPi rate) * 100 + 120))
    , stroke strokeValue
    , strokeWidth strokeWidthValue
    ]
    []

rateToPi: Float -> Float
rateToPi rate =
  pi * 2 * (rate - 0.25) 

send : msg -> Cmd msg
send msg =
  Task.succeed msg
  |> Task.perform identity