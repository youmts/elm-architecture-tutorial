module MyRandom exposing (main)

import Browser
import Html exposing (..)
import Html.Events exposing(..)
import Random

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
  
init : () -> (Model, Cmd Msg)
init _ =
  ( Model 1 1
  , Cmd.none
  )

type Msg
    = Roll
    | NewFace Model

die : Random.Generator Int
die = 
  Random.int 1 6

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
