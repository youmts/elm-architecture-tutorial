module Form exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)

-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
  { name : String
  , age : String
  , password : String
  , passwordAgain : String
  , submitted : Bool
  }


init : Model
init =
  { name = ""
  , age = ""
  , password = ""
  , passwordAgain = ""
  , submitted = False
  }



-- UPDATE


type Msg
  = Name String
  | Age String
  | Password String
  | PasswordAgain String
  | Submit

update : Msg -> Model -> Model
update msg model =
  case msg of
    Name name ->
      { model | name = name }

    Age age ->
      { model | age = age }

    Password password ->
      { model | password = password }

    PasswordAgain password ->
      { model | passwordAgain = password }

    Submit ->
      { model | submitted = True}

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ viewInput "text" "Name" model.name Name
    , viewInput "age" "Age" model.age Age
    , viewInput "password" "Password" model.password Password
    , viewInput "password" "Re-enter Password" model.passwordAgain PasswordAgain
    , viewValidation model
    , button [ onClick Submit ] [ text "submit" ]
    ]

viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg = 
  input [ type_ t, placeholder p, value v, onInput toMsg ] []

viewValidation : Model -> Html msg
viewValidation model =
  if not model.submitted then
    div [ ] [ ]
  else if String.toInt model.age == Nothing then
    viewNG "Age is not number!"
  else if String.length model.password < 8 then
    viewNG "Password leth than 8 characters!"
  else if not (String.any Char.isDigit model.password) then
    viewNG "Password must contain digit!"
  else if not (String.any Char.isUpper model.password) then
    viewNG "Password must contain upper character!"
  else if not (String.any Char.isLower model.password) then
    viewNG "Password must contain lower character!"
  else if model.password /= model.passwordAgain then
    viewNG "Password do not match!"
  else
    div [ style "color" "green" ] [ text "OK" ]

viewNG : String -> Html msg
viewNG message = 
  div [ style "color" "red" ] [ text message]