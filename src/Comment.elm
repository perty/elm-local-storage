module Comment exposing (main)

import Html exposing (Html, div, text, form, textarea, button, input)
import Html.Attributes exposing (type_, action, value, disabled)
import Html.Events exposing (onSubmit, onInput)
import Http
import Json.Decode as Json
import Json.Encode


type alias Model =
    { newComment : NewComment
    , comments : List Comment
    }


emptyModel : Model
emptyModel =
    { newComment = emptyNewComment
    , comments = []
    }


emptyNewComment =
    NewComment -1 "" ""


type alias NewComment =
    { userId : Int
    , title : String
    , body : String
    }


type Msg
    = AddComment
    | UpdateComment NewComment
    | AddCommentHttp (Result Http.Error Comment)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddComment ->
            let
                newComment =
                    Debug.log "model.newComment" model.newComment
            in
                ( { model | newComment = emptyNewComment }, postComment newComment )

        UpdateComment newComment ->
            ( { model | newComment = newComment }, Cmd.none )

        AddCommentHttp (Ok response) ->
            let
                _ =
                    Debug.log "response" response
            in
                ( { model | comments = model.comments ++ [ response ] }, Cmd.none )

        AddCommentHttp (Err err) ->
            let
                _ =
                    Debug.log "err" err
            in
                ( model, Cmd.none )


postComment newComment =
    Http.send AddCommentHttp
        (Http.post "https://jsonplaceholder.typicode.com/posts"
            (encodeNewComment newComment)
            decodeComment
        )


encodeNewComment : NewComment -> Http.Body
encodeNewComment newComment =
    Http.jsonBody <|
        Json.Encode.object
            [ ( "title", Json.Encode.string newComment.title )
            , ( "body", Json.Encode.string newComment.body )
            , ( "userId", Json.Encode.int newComment.userId )
            ]


type alias Comment =
    { title : String
    , body : String
    , userId : Int
    , id : Int
    }


decodeComment : Json.Decoder Comment
decodeComment =
    Json.map4 Comment
        (Json.field "title" Json.string)
        (Json.field "body" Json.string)
        (Json.field "userId" Json.int)
        (Json.field "id" Json.int)


view : Model -> Html Msg
view model =
    div [] <|
        [ viewForm model.newComment UpdateComment AddComment
        ]
            ++ List.map (\comment -> div [] [ text <| toString comment ]) model.comments


viewForm : NewComment -> (NewComment -> msg) -> msg -> Html msg
viewForm newComment toUpdateComment addComment =
    form
        [ onSubmit addComment, action "javascript:void(0);" ]
        [ div []
            [ input
                [ value newComment.title
                , onInput (\v -> toUpdateComment { newComment | title = v })
                ]
                []
            ]
        , textarea
            [ value newComment.body
            , onInput (\v -> toUpdateComment { newComment | body = v })
            ]
            []
        , div []
            [ button
                [ type_ "submit"
                , disabled <| isEmpty newComment.title || isEmpty newComment.body
                ]
                [ text "Add Comment" ]
            ]
        ]


isEmpty : String -> Bool
isEmpty =
    String.isEmpty << String.trim


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , init = ( emptyModel, Cmd.none )
        }