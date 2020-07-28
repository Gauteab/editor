module Editor exposing (..)

import Action exposing (Action(..))
import Element exposing (Element, row, text)
import Elm.Parser
import Elm.Processing as Processing
import Elm.RawFile exposing (RawFile)
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Expression(..))
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Node as N exposing (Node(..))
import State exposing (State)
import Tree exposing (Tree)
import Tree.Zipper as Zipper exposing (Zipper)



-- MODEL


type alias Editor =
    { nextId : Int
    , zipper : Zipper Node
    }


type alias Node =
    { id : Int
    , text : String
    , tag : String
    }


type alias Ast =
    Tree Node


example : String
example =
    """
module Test exposing (..)
food = if x then y else [z,2]
"""


init =
    let
        ( i, tree ) =
            parse example
                |> relabel 0
                |> Debug.log "tree"
    in
    Editor i (Zipper.fromTree tree)
        |> steps []



-- UPDATE


step : Action -> Editor -> Editor
step action { nextId, zipper } =
    case action of
        SelectParent ->
            Editor nextId (Zipper.parent zipper |> Maybe.withDefault zipper)

        NextSibling ->
            Editor nextId (Zipper.nextSibling zipper |> Maybe.withDefault zipper)

        PreviousSibling ->
            Editor nextId (Zipper.previousSibling zipper |> Maybe.withDefault zipper)

        FirstChild ->
            Editor nextId (Zipper.firstChild zipper |> Maybe.withDefault zipper)

        LastChild ->
            Editor nextId (Zipper.lastChild zipper |> Maybe.withDefault zipper)

        Delete ->
            Editor nextId zipper

        Lift ->
            Editor nextId zipper

        ReverseChildren ->
            Editor nextId zipper


steps : List Action -> Editor -> Editor
steps actions editor =
    List.foldl step editor actions


relabel : Int -> Ast -> ( Int, Ast )
relabel initial tree =
    Tree.mapAccumulate (\a l -> ( a + 1, { l | id = a } )) initial tree



-- VIEW


view : Editor -> Element msg
view editor =
    viewTree (Zipper.tree editor.zipper)


viewTree tree =
    let
        children =
            Tree.children tree

        label =
            Tree.label tree
    in
    if not <| List.isEmpty children then
        row [] <|
            (text <| "(" ++ label.tag ++ " ")
                :: (List.map viewTree children
                        ++ [ text ")" ]
                   )

    else
        text (String.join "" [ "", label.text, " " ])



-- PARSER


parse : String -> Tree Node
parse input =
    case Elm.Parser.parse input of
        Err e ->
            Debug.todo (Debug.toString e)

        Ok v ->
            processRawFile v


processRawFile : Elm.RawFile.RawFile -> Tree Node
processRawFile rawFile =
    let
        node v =
            Tree.tree (Node 0 "" v)

        leaf v t =
            Tree.singleton (Node 0 v t)

        file : File
        file =
            Processing.process Processing.init rawFile

        processExpression : Expression -> Tree Node
        processExpression e =
            case e of
                ListExpr es ->
                    node "list" (List.map (processExpression << N.value) es)

                IfBlock e1 e2 e3 ->
                    node "if" (List.map (processExpression << N.value) [ e1, e2, e3 ])

                Integer i ->
                    leaf (String.fromInt i) "int"

                FunctionOrValue _ s ->
                    leaf s "id"

                _ ->
                    Debug.todo (Debug.toString e)

        processFile : File -> List (Tree Node)
        processFile =
            .declarations >> List.map (N.value >> processDecoration)

        processDecoration : Declaration -> Tree Node
        processDecoration declaration =
            case declaration of
                FunctionDeclaration function ->
                    let
                        implementation =
                            N.value function.declaration
                    in
                    node "assignment"
                        [ leaf (N.value implementation.name) "id"
                        , processExpression (N.value implementation.expression)
                        ]

                _ ->
                    Debug.todo ""
    in
    node "module" <| processFile file
