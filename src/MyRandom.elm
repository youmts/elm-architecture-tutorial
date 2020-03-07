module MyRandom exposing (main)

import Browser
import Html exposing (..)
import Html.Events exposing(..)
import Random
import Task

main = 
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

type alias Model = 
  { dieFaceOne : Int
  , dieFaceTwo : Int
  }
  
-- https://medium.com/elm-shorts/how-to-turn-a-msg-into-a-cmd-msg-in-elm-5dd095175d84
init : () -> (Model, Cmd Msg)
init _ =
  ( Model 1 1
  , send Roll
  )

type Msg
    = Roll
    | NewFace Model

die : Random.Generator Int
die = 
  Random.weighted
    (10, 1)
    [ (10, 2)
    , (10, 3)
    , (50, 4)
    , (50, 5)
    , (50, 6)
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
    [ h1 [] [ text (String.fromInt model.dieFaceOne ) ]
    , h1 [] [ text (String.fromInt model.dieFaceTwo ) ]
    , button [ onClick Roll ] [ text "Roll" ]
    ]

send: Msg -> Cmd Msg
send msg =
  Task.succeed msg
  |> Task.perform identity
