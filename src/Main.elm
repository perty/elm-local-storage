port module Main exposing (..)

import Ports.LocalStorage
import Json.Decode
import Json.Encode
import Navigation
import Html exposing (Html, form, h1, div, input, text)
import Html.Attributes exposing (class, value)
import Html.Events exposing (onSubmit, onInput)


main : Program Never Model Msg
main =
    Navigation.program urlParser
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { lastSearch : String
    }


type Msg
    = UpdateSearch String
    | SaveSearch String
    | RequestLastSearch
    | ReceiveFromLocalStorage ( String, Json.Decode.Value )


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    ( { lastSearch = "" }, Cmd.none )



-- PARSING
-- The URL parser mentioned in the program entry point. Takes a Location and
-- parse it to see where to go next.


urlParser : Navigation.Location -> Msg
urlParser location =
    RequestLastSearch



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateSearch s ->
            ( { model | lastSearch = s }, Cmd.none )

        SaveSearch searchQuery ->
            ( model
            , Ports.LocalStorage.storageSetItem ( "lastSearch", Json.Encode.string searchQuery )
            )

        RequestLastSearch ->
            ( model, Ports.LocalStorage.storageGetItem "lastSearch" )

        ReceiveFromLocalStorage ( "lastSearch", value ) ->
            case Json.Decode.decodeValue Json.Decode.string value of
                Ok searchQuery ->
                    ( model, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        ReceiveFromLocalStorage _ ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "overwrite-container" ]
        [ h1 [] [ text ("Header") ]
        , form [ onSubmit (SaveSearch model.lastSearch) ]
            [ input [ value model.lastSearch, onInput (\v -> UpdateSearch v) ] []
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Ports.LocalStorage.storageGetItemResponse ReceiveFromLocalStorage
