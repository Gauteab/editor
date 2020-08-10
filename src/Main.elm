module Main exposing (main)

import Action exposing (Action(..))
import Browser
import Browser.Events
import Dict
import Editor exposing (Editor)
import Element exposing (Element, alignRight, column, el, explain, fill, fillPortion, height, row, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html)
import Json.Decode
import Keyboard.Event exposing (KeyboardEvent, decodeKeyboardEvent)
import Keyboard.Key as Key



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
    { editor : Editor
    , mode : Mode
    }


type Mode
    = Normal
    | Insert String


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Editor.init Normal
    , Cmd.none
    )



-- UPDATE


type Msg
    = NoOp
    | KeyboardEvent KeyboardEvent


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

        KeyboardEvent keyboardEvent ->
            let
                key =
                    Maybe.withDefault "" keyboardEvent.key
            in
            case model.mode of
                Normal ->
                    let
                        newEditor =
                            case Dict.get key keyActionMap of
                                Just action ->
                                    Editor.step action model.editor

                                _ ->
                                    model.editor
                    in
                    case key of
                        "i" ->
                            ( { model
                                | mode = Insert ""
                              }
                            , Cmd.none
                            )

                        _ ->
                            ( { model
                                | editor = newEditor
                              }
                            , Cmd.none
                            )

                Insert string ->
                    case keyboardEvent.keyCode of
                        Key.Escape ->
                            ( { model | mode = Normal }, Cmd.none )

                        Key.Backspace ->
                            ( { model
                                | mode = Insert <| String.slice 0 -1 string
                                , editor = Editor.step (InsertText <| String.slice 0 -1 string) model.editor
                              }
                            , Cmd.none
                            )

                        _ ->
                            case String.uncons key of
                                Just ( c, "" ) ->
                                    let
                                        s =
                                            string ++ String.fromChar c
                                    in
                                    ( { model
                                        | editor = Editor.step (InsertText s) model.editor
                                        , mode = Insert s
                                      }
                                    , Cmd.none
                                    )

                                _ ->
                                    ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onKeyDown <| Json.Decode.map KeyboardEvent decodeKeyboardEvent



-- VIEW


view : Model -> Html Msg
view model =
    Element.layout
        [ Font.family [ Font.monospace ]
        , Font.color (Element.rgb255 50 50 50)
        , Background.color (Element.rgb255 247 247 247)
        , Element.paddingXY 15 15
        , width fill
        , height fill
        ]
    <|
        row [ width fill, height fill ]
            [ editorPanel model.editor
            , sidePanel (Editor.getFocusedTag model.editor) model.mode
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


sidePanel : String -> Mode -> Element msg
sidePanel tag mode =
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
        [ text "focus:"
        , text tag
        , text "mode:"
        , text (Debug.toString mode)
        ]
