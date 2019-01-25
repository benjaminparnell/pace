module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, div, h1, input, text)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onInput)
import Round


type alias Model =
    { distance : Float
    , time : Float
    , pace : Maybe Float
    }


type Msg
    = UpdateTime String
    | UpdateDistance String
    | UpdatePace


init : () -> ( Model, Cmd msg )
init never =
    ( { distance = 0, time = 0, pace = Nothing }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        UpdateTime value ->
            ( { model | time = String.toFloat value |> Maybe.withDefault 0 } |> updatePace, Cmd.none )

        UpdateDistance value ->
            ( { model | distance = String.toFloat value |> Maybe.withDefault 0 } |> updatePace, Cmd.none )

        UpdatePace ->
            ( model, Cmd.none )


updatePace : Model -> Model
updatePace model =
    if model.distance > 0 && model.time > 0 then
        { model | pace = (model.time / model.distance) |> Just }

    else
        { model | pace = Nothing }


paceText : Maybe Float -> String
paceText pace =
    case pace of
        Just paceValue ->
            "Pace: " ++ Round.round 2 paceValue

        Nothing ->
            "Pace"


view : Model -> Browser.Document Msg
view model =
    { title = "Pace"
    , body =
        [ div []
            [ h1 [] [ text (paceText model.pace) ]
            , input [ placeholder "Distance", value (String.fromFloat model.distance), onInput UpdateDistance ] []
            , input [ placeholder "Time", value (String.fromFloat model.time), onInput UpdateTime ] []
            ]
        ]
    }


subscriptions model =
    Sub.none


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
