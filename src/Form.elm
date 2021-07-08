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
  , age : Int
  , password : String
  }

type alias ViewModel =
  { name : String
  , age : String
  , password : String
  , passwordAgain : String
  , result : Maybe (Result String Model)
  }


init : ViewModel
init =
  { name = ""
  , age = ""
  , password = ""
  , passwordAgain = ""
  , result = Nothing
  }



-- UPDATE


type Msg
  = Name String
  | Age String
  | Password String
  | PasswordAgain String
  | Submit

update : Msg -> ViewModel -> ViewModel
update msg vm =
  case msg of
    Name name ->
      { vm | name = name }

    Age age ->
      { vm | age = age }

    Password password ->
      { vm | password = password }

    PasswordAgain password ->
      { vm | passwordAgain = password }

    Submit ->
      { vm | result = Just (toModel vm) }

-- VIEW

view : ViewModel -> Html Msg
view vm =
  div []
    [ viewInput "text" "Name" vm.name Name
    , viewInput "age" "Age" vm.age Age
    , viewInput "password" "Password" vm.password Password
    , viewInput "password" "Re-enter Password" vm.passwordAgain PasswordAgain
    , viewValidation vm.result
    , button [ onClick Submit ] [ text "submit" ]
    ]

viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg = 
  input [ type_ t, placeholder p, value v, onInput toMsg ] []

viewValidation : Maybe (Result String Model) -> Html msg
viewValidation result =

  case result of
    Nothing -> 
      div [ ] [ ]
    Just justResult ->
      case justResult of
        Ok _ ->
          div [ style "color" "green" ] [ text "OK" ]
        Err message ->
          viewNG message

toModel : ViewModel -> Result String Model
toModel vm = 
  case String.toInt vm.age of
    Nothing ->
      Err "Age is not number!"
    Just ageInt ->
      if String.length vm.password < 8 then
        Err "Password leth than 8 characters!"
      else if not (String.any Char.isDigit vm.password) then
        Err "Password must contain digit!"
      else if not (String.any Char.isUpper vm.password) then
        Err "Password must contain upper character!"
      else if not (String.any Char.isLower vm.password) then
        Err "Password must contain lower character!"
      else if vm.password /= vm.passwordAgain then
        Err "Password do not match!"
      else
        Ok (Model vm.name ageInt vm.password)

viewNG : String -> Html msg
viewNG message = 
  div [ style "color" "red" ] [ text message]