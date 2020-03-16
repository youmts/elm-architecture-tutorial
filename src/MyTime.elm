module MyTime exposing (main)

import Browser
import Html exposing (..)
import Task
import Time 
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)

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
  div[]
  [
    h1
      [ style "display" "inline-block"
      , style "border" "solid 2px #aaaaaa"
      , style "border-radius" "10px"
      ]
      [ text (hour ++ ":" ++ minute ++ ":" ++ second)
      ]
    , button [ onClick TogglePause ] [ text "toggle pause" ]
  ]

send : msg -> Cmd msg
send msg =
  Task.succeed msg
  |> Task.perform identity