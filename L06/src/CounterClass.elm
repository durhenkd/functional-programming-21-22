
module CounterClass exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)
import Html.Attributes exposing (disabled)
import Html.Attributes exposing (class)

main =
  Browser.sandbox { init = 0, update = update, view = view }

type alias Model = Int

type Msg = Increment | Decrement

update : Msg -> Model -> Model
update msg model =
  case msg of
    Increment -> if model >= 10 then model else model + 1
    Decrement -> if model <= -10 then model else model - 1

view : Model -> Html Msg
view model =
  let
    bigFont = style "font-size" "20pt"
    textStyle = if model >= 8 || model <= -8 then [style "font-size" "20pt", style "color" "red"] else [style "font-size" "20pt"]
    incDisabled = if model >= 10 then True else False
    decDisabled = if model <= -10 then True else False
  in
    div []
      [ button [ bigFont, onClick Increment, disabled (incDisabled) ] [ text "+" ]
      , div textStyle [ text (String.fromInt model) ]
      , button [ bigFont, onClick Decrement, disabled (decDisabled) ] [ text "-" ]
      ]

