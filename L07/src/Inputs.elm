
module Inputs exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (checked, placeholder, style, type_, value, disabled)
import Html.Events exposing (..)
import List

main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type AccountType
    = User
    | Admin


accountTypes : List AccountType
accountTypes = [User, Admin]

accountTypeToString ty =
    case ty of
        User ->
            "User"

        Admin ->
            "Admin"


accountTypeFromString s =
    case String.toLower s of
        "user" ->
            Just User

        "admin" ->
            Just Admin

        _ ->
            Nothing


type alias Model =
    { accountType : AccountType
    , activateAccount : Bool
    , username : String
    , password : String
    , passwordC : String
    , emailAddress : Maybe String
    }


type Msg
    = SelectedValue String
    | UsernameChanged String
    | PasswordChanged String
    | PasswordCChanged String  
    | SetActivateAccount Bool

init : () -> ( Model, Cmd Msg )
init _ =
    ( { accountType = User, activateAccount = False, username = "", password = "", passwordC = "", emailAddress = Nothing }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectedValue s ->
            ( { model | accountType = accountTypeFromString s |> Maybe.withDefault User }
            , Cmd.none
            )

        UsernameChanged username ->
            ( { model | username = username }
            , Cmd.none
            )

        PasswordChanged password ->
            ( { model | password = password }
            , Cmd.none
            )

        PasswordCChanged passwordc ->
            ( { model | passwordC = passwordc }
            , Cmd.none
            )

        SetActivateAccount activate ->
            ( { model | activateAccount = activate }
            , Cmd.none
            )


accountTypeView : Html Msg
accountTypeView =
    div []
        [ select [ Html.Events.onInput SelectedValue ]
            [ option [ value "User" ] [ text "User" ]
            , option [ value "Admin" ] [ text "Admin" ]
            ]
        ]


accountDetailsView : Model -> Html Msg
accountDetailsView { username, password, passwordC } =
    let
        inputAttrs ty p v msg =
            [ type_ ty, placeholder p, value v, onInput msg ]
    in
    div []
        [ input (inputAttrs "text" "username" username UsernameChanged) []
        , input (inputAttrs "password" "password" password PasswordChanged) []
        , input (inputAttrs "password" "password again" passwordC PasswordCChanged) []
        ]


activateAccountView : Bool -> Html Msg
activateAccountView yes =
    div []
        [ input [ type_ "checkbox", onCheck SetActivateAccount, checked yes ] []
        , text "Activate account?"
        ]


statusView : Model -> Html Msg
statusView model =
    div []
        [ p [] [ text "Account type: ", text <| accountTypeToString model.accountType ]
        , p [] [ text "Username: ", text model.username ]
        , p [] [ text "Password: ", text model.password ]
        , p []
            [ text <|
                if model.activateAccount then
                    "Account will be created activated"

                else
                    "Account will be created suspended"
            ]
        ]

passCheckView : Model -> Html Msg
passCheckView { accountType, password, passwordC } = 
    let
        getText color tex = h4 [style "color" color] [text tex]
        passMatchy = if password == "" || passwordC == "" then getText "red" "At least a password field is empty" else
                    if password /= passwordC then getText "red" "Passwords do not match" else
                    getText "green" "Passwords match!"
        passLength = (password |> String.toList |> List.length)
    in
        if accountType == User then if passLength < 8 then getText "red" "Password should be at least 8 characters!" else passMatchy else
                                    if passLength < 12 then getText "red" "Password should be at least 12 characters!" else passMatchy

createAccBtnView : Model -> Html Msg
createAccBtnView model =
    let
        isDisabled = if model.username == "" || model.password == "" || (model.password /= model.passwordC) then True else False
    in
    button [disabled (isDisabled)] [text "Create Account"]

view : Model -> Html Msg
view model =
    div []
        [ statusView model
        , accountTypeView
        , activateAccountView model.activateAccount
        , accountDetailsView model
        , passCheckView model
        , createAccBtnView model
        ]

