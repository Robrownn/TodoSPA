module Main exposing (Model, Msg(..), TodoItem, decodeTodo, decodeTodos, encodeTodo, getTodo, handleError, init, main, postNewTask, put, renderInput, renderTodos, subscriptions, todoApi, update, view)

import Array
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder, field, map3, string)
import Json.Encode as Encode
import Url.Builder as Url


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



--MODEL


type alias TodoItem =
    { id : Int
    , name : String
    , isComplete : Bool
    }


type alias Model =
    { todoItems : List TodoItem
    , message : String
    , newTodo : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model [] "Click da button" ""
    , getTodo
    )



--UPDATE


handleError : Model -> Http.Error -> ( Model, Cmd Msg )
handleError model result =
    case result of
        Http.BadUrl errMsg ->
            ( { model | message = errMsg }
            , Cmd.none
            )

        Http.Timeout ->
            ( { model | message = "Request timed out" }
            , Cmd.none
            )

        Http.NetworkError ->
            ( { model | message = "Get outta the cave you dingus" }
            , Cmd.none
            )

        Http.BadStatus errMsg ->
            ( { model | message = String.fromInt errMsg.status.code ++ ": " ++ errMsg.status.message }
            , Cmd.none
            )

        Http.BadPayload debugMsg response ->
            ( { model | message = debugMsg ++ "\n" ++ String.fromInt response.status.code ++ ": " ++ response.status.message }
            , Cmd.none
            )


type Msg
    = GetTodo
    | TodoList (Result Http.Error (List TodoItem))
    | NewTodo (Result Http.Error TodoItem)
    | UpdateTodo (Result Http.Error ())
    | PostTodo
    | Change String
    | ChangeDone Int Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetTodo ->
            ( model
            , getTodo
            )

        TodoList result ->
            case result of
                Ok todos ->
                    ( { model | todoItems = todos, message = "" }
                    , Cmd.none
                    )

                Err errResult ->
                    handleError model errResult

        NewTodo result ->
            case result of
                Ok _ ->
                    ( model
                    , getTodo
                    )

                Err errResult ->
                    handleError model errResult

        UpdateTodo result ->
            case result of
                Ok _ ->
                    ( model
                    , getTodo
                    )

                Err errResult ->
                    handleError model errResult

        PostTodo ->
            ( model
            , postNewTask model.newTodo
            )

        Change newItem ->
            ( { model | newTodo = newItem }
            , Cmd.none
            )

        ChangeDone id value ->
            ( model
            , jsonPatchComplete id (not value)
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



--VIEW


renderTodos : List TodoItem -> Html Msg
renderTodos lst =
    ul []
        (List.map
            (\l ->
                li []
                    [ div []
                        [ text (l.name ++ "\t")
                        , input [ type_ "checkbox", onClick (ChangeDone l.id l.isComplete) ] []
                        ]
                    ]
            )
            lst
        )


renderInput : Model -> Html Msg
renderInput model =
    div []
        [ input [ placeholder "Your todo", value model.newTodo, onInput Change ] []
        , button [ onClick PostTodo ] [ text "New Todo" ]
        ]


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ renderTodos model.todoItems ]
        , div [] [ text model.message ]
        , renderInput model
        ]



-- HTTP


put : String -> Http.Body -> Http.Request ()
put url body =
    Http.request
        { method = "PUT"
        , headers = []
        , url = url
        , body = body
        , expect = Http.expectStringResponse (\_ -> Ok ())
        , timeout = Nothing
        , withCredentials = False
        }


patch : String -> Http.Body -> Http.Request ()
patch url body =
    Http.request
        { method = "PATCH"
        , headers = []
        , url = url
        , body = body
        , expect = Http.expectStringResponse (\_ -> Ok ())
        , timeout = Nothing
        , withCredentials = False
        }


jsonPatchComplete : Int -> Bool -> Cmd Msg
jsonPatchComplete id complete =
    Http.send UpdateTodo (patch (todoApiAt id) (Http.jsonBody (encodeJsonArray encodePatchComplete (List.singleton complete))))



-- sendNewName : String -> Cmd Msg
-- sendNewName name =
--     Http.send NewName (put helloApi (Http.jsonBody (encodeName name)) )


postNewTask : String -> Cmd Msg
postNewTask name =
    Http.send NewTodo (Http.post todoApi (Http.jsonBody (encodeTodo name)) decodeTodo)


encodeTodo : String -> Encode.Value
encodeTodo name =
    Encode.object
        [ ( "name", Encode.string name )
        , ( "isComplete", Encode.bool False )
        ]


encodeJsonArray : (Bool -> Encode.Value) -> List Bool -> Encode.Value
encodeJsonArray function object =
    Encode.array function (Array.fromList object)


encodePatchComplete : Bool -> Encode.Value
encodePatchComplete complete =
    Encode.object
        [ ( "op", Encode.string "replace" )
        , ( "path", Encode.string "/isComplete" )
        , ( "value", Encode.bool complete )
        ]


getTodo : Cmd Msg
getTodo =
    Http.send TodoList (Http.get todoApi decodeTodos)


todoApi : String
todoApi =
    Url.crossOrigin "http://localhost:5000"
        [ "api", "todo" ]
        []


todoApiAt : Int -> String
todoApiAt id =
    Url.crossOrigin "http://localhost:5000"
        [ "api", "todo", String.fromInt id ]
        []


decodeTodo : Decode.Decoder TodoItem
decodeTodo =
    map3 TodoItem
        (field "id" Decode.int)
        (field "name" Decode.string)
        (field "isComplete" Decode.bool)


decodeTodos : Decode.Decoder (List TodoItem)
decodeTodos =
    Decode.list decodeTodo
