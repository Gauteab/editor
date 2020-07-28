module Main exposing (main)

import Action
import Browser
import Editor
import Element exposing (Element)
import Element.Font as Font
import Html exposing (Html)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { editor : Editor.Editor }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { editor = Editor.init }
    , Cmd.none
    )



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    Element.layout
        [ Font.family [ Font.monospace ]
        , Font.color (Element.rgb255 0 0 0)
        , Element.paddingXY 5 5
        ]
    <|
        Editor.view
            model.editor
