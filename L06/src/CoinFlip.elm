
module CoinFlip exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (..)
import Random

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

type CoinSide
  = Heads
  | Tails

type alias Model =
  { currentFlip : Maybe CoinSide
  , heads : Int                                                                                         -- code for E 6.6.1
  , tails : Int                                                                                         -- code for E 6.6.1
  , flips: List CoinSide 
  }

initModel = Model Nothing 0 0 [] 

init : () -> (Model, Cmd Msg)
init _ =
  ( initModel
  , Random.generate AddFlip coinFlip                                                                    -- code for E 6.6.2
  )

type Msg
  = Flip
  | Flip10                                                                                              -- code for E 6.6.3
  | Flip100                                                                                             -- code for E 6.6.3
  | AddFlip CoinSide 
  | AddFlips (List (CoinSide))                                                                          -- code for E 6.6.3

countSides : List (CoinSide) -> CoinSide -> Int                                                         -- code for E 6.6.1
countSides list side = list |> List.filter (\x -> if x == side then True else False) |> List.length     -- code for E 6.6.1

newHeadCount : CoinSide -> Int -> Int
newHeadCount c nr = if c == Heads then nr + 1 else nr                                               -- code for E 6.6.1

newTailCount : CoinSide -> Int -> Int
newTailCount c nr = if c == Tails then nr + 1 else nr                                               -- code for E 6.6.1

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Flip ->
      ( model
      , Random.generate AddFlip coinFlip
      )
    -- code for E 6.6.3
    Flip10 -> 
      ( model
      , Random.generate AddFlips (Random.list 10 coinFlip)
      )
    -- code for E 6.6.3
    Flip100 ->
      ( model
      , Random.generate AddFlips (Random.list 100 coinFlip)
      )

    AddFlip coin ->
      ( Model (Just coin) (model.heads |> newHeadCount coin) (model.tails |> newTailCount coin) (coin::model.flips)
      , Cmd.none
      )
    -- code for E 6.6.3
    AddFlips coins ->
      ( Model (coins |> List.head |> Maybe.withDefault Heads |> Just) (model.heads + (countSides coins Heads)) (model.tails + (countSides coins Tails)) (coins ++ model.flips)
      , Cmd.none
      )

coinFlip : Random.Generator CoinSide
coinFlip =
  Random.uniform Heads
    [ Tails ]

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

view : Model -> Html Msg
view model =
  let
    currentFlip = 
      model.currentFlip 
      |> Maybe.map viewCoin
      |> Maybe.withDefault (text "Press the flip button to get started")
    flips = 
      model.flips 
      |> List.map coinToString
      |> List.intersperse " "
      |> List.map text
    scoreboard1 = text ("From Model -> Tails: " ++ String.fromInt(model.tails) ++ " | Heads: " ++ String.fromInt(model.heads))  -- code for E 6.6.1
    scoreboard2 = text ("From List -> Tails: " ++ String.fromInt(countSides model.flips Tails) ++ " | Heads: " ++ String.fromInt(countSides model.flips Heads))-- code for E 6.6.1
  in
    div []
      [ button [ onClick Flip ] [ text "Flip" ]
      , button [ onClick Flip10 ] [ text "Flip 10" ]
      , button [ onClick Flip100 ] [ text "Flip 100" ]
      , currentFlip
      , scoreboard1 -- code for E 6.6.1
      , div [] flips
      , scoreboard2 -- code for E 6.6.1
      ]

coinToString : CoinSide -> String
coinToString coin =
  case coin of
    Heads -> "h"
    Tails -> "t"

viewCoin : CoinSide -> Html Msg
viewCoin coin =
  let
    name = coinToString coin
  in
    div [ style "font-size" "4em" ] [ text name ]

