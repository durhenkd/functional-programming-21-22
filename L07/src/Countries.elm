
module Countries exposing (..)

import List
import Browser
import Html exposing (..)
import Html.Attributes exposing (style, value, checked, placeholder, type_)
import Html.Events exposing (..)
import Http
import Json.Decode as Dec


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

type alias Country =
    { name : String
    , area : Float
    , region : String
    , population : Int
    }


decodeCountry : Dec.Decoder Country
decodeCountry =
    Dec.map4 Country 
        (Dec.at  ["name", "common"] Dec.string)
        (Dec.field "area" Dec.float)
        (Dec.field "region" Dec.string)
        (Dec.field "population" Dec.int)

type Model
    = Initial
    | RequestSent
    | Success { countries: (List Country), field: String, ascending : Bool }
    | Error Http.Error



init : () -> ( Model, Cmd Msg )
init _ =
    ( Initial
    , Cmd.none
    )


type Msg
    = GetCountries
    | GotCountries (Result Http.Error (List Country))
    | SortBy String
    | ChangeOrder Bool

getCountries : Cmd Msg
getCountries = Http.get 
    { url = "https://restcountries.com/v3.1/all"
    , expect = Http.expectJson GotCountries (Dec.list decodeCountry) 
    }

sortCountries : String -> Bool -> List Country -> List Country
sortCountries field ascending countries = 
    let
        sort = 
            case field of
                "Population" -> countries |> List.sortBy (\x -> x.population)
                "Area" ->       countries |> List.sortBy (\x -> x.area)
                "Density" ->    countries |> List.sortBy (\x -> (toFloat <| x.population)/x.area)
                _ -> countries
    in
    if ascending then sort else sort |> List.reverse
    

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of

        SortBy value ->
            case model of 
                Success record -> ({record | countries = record.countries |> sortCountries value record.ascending, field = value } |> Success, Cmd.none)
                _ ->  (model, Cmd.none)
                
        ChangeOrder value ->
            case model of 
                Success record -> ({record | countries = record.countries |> sortCountries record.field value , ascending = value } |> Success, Cmd.none)
                _ ->  (model, Cmd.none)

        GetCountries ->
            ( RequestSent
            , getCountries
            )

        GotCountries (Ok countries) ->
            ( ({countries = countries |> sortCountries "Population" False, field =  "Population", ascending = False} |> Success), Cmd.none) 
            

        GotCountries (Err err) ->
            ( Error err
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



view : Model -> Html Msg
view model =
    case model of
        Initial ->
            viewInitial

        RequestSent ->
            div [] [ text "Loading..." ]

        Success countries ->
            viewSuccess countries.countries

        Error err ->
            viewError err

viewInitial : Html Msg
viewInitial =
    div []
        [ button [ onClick GetCountries ] [ text "Get countries" ]
        ]

viewCountry : Country -> Html msg
viewCountry {name, area, region, population} =
    div [style "border" "solid 1px", style "margin" "2px"] 
        [ p [] [text <| "Name:" ++ name]
        , p [] [text <| "Area: " ++ String.fromFloat area ++ " sqkm"]
        , p [] [text <| "Population: " ++ String.fromInt population]
        , p [] [text <| "Population Density: " ++ String.fromFloat ((toFloat <| population)/area) ++ " p/sqkm"]
        ]

viewSorting : Html Msg
viewSorting = div [] 
    [ select [ Html.Events.onInput SortBy ] 
        [ option [ value "Population" ] [text "Population"]
        , option [ value "Area" ] [text "Area"]
        , option [ value "Density"]  [text "Density"]
        ]
    , input [ type_ "checkbox", onCheck ChangeOrder] []
    , text "Ascending"
    ]


viewSuccess : List Country -> Html Msg
viewSuccess countries =
    div [] ((h2 [] [ text "ok" ])::(viewSorting::List.map viewCountry countries))

httpErrorToString : Http.Error -> String
httpErrorToString err =
    case err of
        Http.BadUrl _ ->
            "Bad Url"

        Http.Timeout ->
            "Timeout"

        Http.NetworkError ->
            "Network Error"

        Http.BadStatus status ->
            "BadS tatus: " ++ String.fromInt status

        Http.BadBody _ ->
            "Bad Body"


viewError : Http.Error -> Html msg
viewError err =
    div [] [ h2 [] [ text "Rip" ], p [] [ text <| httpErrorToString err ] ]


    

