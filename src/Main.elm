module Main exposing (main)

import Action exposing (Action(..))
import Browser
import Browser.Events
import Dict
import Editor exposing (Editor)
import Element exposing (Element, alignRight, column, el, explain, fill, fillPortion, height, row, text, width)
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html)
import Json.Decode
import Keyboard.Event exposing (KeyboardEvent, decodeKeyboardEvent)



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
    | HandleKeyboardEvent KeyboardEvent


keyActionMap =
    Dict.fromList <|
        [ ( "j", FirstChild )
        , ( "k", SelectParent )
        , ( "h", PreviousSibling )
        , ( "l", NextSibling )
        , ( "w", LastChild )
        , ( "d", Delete )
        , ( "K", Lift )
        , ( "L", SwapRight )
        , ( "H", SwapLeft )
        , ( "r", ReverseChildren )
        , ( "a", AddNode "hole" )
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        HandleKeyboardEvent keyboardEvent ->
            let
                newEditor =
                    case Dict.get (Maybe.withDefault "" keyboardEvent.key) keyActionMap of
                        Just action ->
                            Editor.step action model.editor

                        _ ->
                            model.editor
            in
            ( { model
                | editor = newEditor
              }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onKeyDown <| Json.Decode.map HandleKeyboardEvent decodeKeyboardEvent



-- VIEW


view : Model -> Html Msg
view model =
    Element.layout
        [ Font.family [ Font.monospace ]
        , Font.color (Element.rgb255 0 0 0)
        , Element.paddingXY 15 15
        , width fill
        , height fill
        ]
    <|
        row [ width fill, height fill ]
            [ editorPanel model.editor
            , sidePanel (Editor.getFocusedTag model.editor)
            ]


editorPanel : Editor -> Element msg
editorPanel editor =
    el
        [ width (fillPortion 4)
        , height fill
        , Element.scrollbarY
        ]
    <|
        Editor.view editor


sidePanel : String -> Element msg
sidePanel tag =
    column
        [ Border.solid
        , Border.width 1
        , Border.widthEach
            { bottom = 0
            , left = 1
            , right = 0
            , top = 0
            }
        , height fill
        , width (fillPortion 1)
        , alignRight
        , Element.padding 5
        ]
        [ text "focus:", text tag ]
