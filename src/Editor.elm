module Editor exposing (..)

import Action exposing (Action(..))
import Dict exposing (Dict)
import Element exposing (Element, alignTop, column, el, explain, paddingEach, rgb255, row, spacing, text)
import Element.Background as Background
import Element.Font as Font
import Elm.Parser
import Elm.Processing as Processing
import Elm.RawFile exposing (RawFile)
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Expression(..), LetDeclaration(..))
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Infix exposing (InfixDirection(..))
import Elm.Syntax.Module as Module
import Elm.Syntax.Node as N exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.Type exposing (ValueConstructor)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation(..))
import Maybe exposing (Maybe)
import Tree exposing (Tree)
import Tree.Zipper as Zipper exposing (Zipper)



-- MODEL


type alias Editor =
    { nextId : Int
    , zipper : Cursor
    }


type alias Node =
    { id : Int
    , text : String
    , tag : String
    }


type alias Ast =
    Tree Node


type alias Cursor =
    Zipper Node


example : String
example =
    """
module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--


import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)



-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model = Int


init : Model
init =
  0



-- UPDATE


type Msg
  = Increment
  | Decrement


update : Msg -> Model -> Model
update msg model =
  case msg of
    Increment ->
      model + 1

    Decrement ->
      model - 1



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ button [ onClick Decrement ] [ text "-" ]
    , div [] [ text (String.fromInt model) ]
    , button [ onClick Increment ] [ text "+" ]
    ]
"""


init : Editor
init =
    let
        ( i, tree ) =
            parse example
                |> relabel 0
                |> Debug.log "tree"
    in
    Editor i (Zipper.fromTree tree)
        |> steps
            [ LastChild
            , FirstChild
            , NextSibling
            , NextSibling
            , NextSibling
            , NextSibling
            ]


getFocusedTag : Editor -> String
getFocusedTag =
    .zipper >> Zipper.label >> .tag



-- UPDATE


steps : List Action -> Editor -> Editor
steps actions editor =
    List.foldl step editor actions


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
            Editor nextId <| Debug.todo ""

        InsertText s ->
            if List.isEmpty <| Zipper.children zipper then
                Editor nextId <|
                    Zipper.mapLabel (\l -> { l | text = s }) zipper

            else
                Debug.todo "Inserted text into non-leaf node"

        AddNode s ->
            case newNode s of
                Just ast ->
                    zipper
                        |> Zipper.append (Debug.log "ast" ast)
                        |> Editor (nextId + 2)

                Nothing ->
                    Editor nextId zipper

        Lift ->
            Zipper.parent zipper
                |> (Maybe.map << Zipper.mapTree << always << Zipper.tree) zipper
                |> Maybe.withDefault zipper
                |> Editor nextId

        SwapRight ->
            swapWith Zipper.nextSibling zipper
                |> Maybe.withDefault zipper
                |> Editor nextId

        SwapLeft ->
            swapWith Zipper.previousSibling zipper
                |> Maybe.withDefault zipper
                |> Editor nextId

        ReverseChildren ->
            zipper
                |> (Zipper.mapTree << Tree.mapChildren) List.reverse
                |> Editor nextId


swapWith : (Cursor -> Maybe Cursor) -> Cursor -> Maybe Cursor
swapWith f zipper =
    let
        swapTwoTrees t1 t2 t =
            if treeId t == treeId t1 then
                t2

            else if treeId t == treeId t2 then
                t1

            else
                t
    in
    f zipper
        |> Maybe.andThen
            (\sibling ->
                Zipper.parent zipper
                    |> (Maybe.map << Zipper.mapTree << Tree.mapChildren << List.map)
                        (swapTwoTrees (Zipper.tree zipper) (Zipper.tree sibling))
                    |> (Maybe.andThen << Zipper.findNext) (\a -> a.id == zipperId zipper)
            )


zipperId : Cursor -> Int
zipperId =
    Zipper.label >> .id


treeId : Ast -> Int
treeId =
    Tree.label >> .id


relabel : Int -> Ast -> ( Int, Ast )
relabel initial tree =
    Tree.mapAccumulate (\a l -> ( a + 1, { l | id = a } )) initial tree



-- VALIDATION
{- A data structure to be used for validating trees and generating a valid trees

   list: many expression
   case: some branch
   if:   one expression followed by an even number (>= 2) of expressions... or should this also be divided into branches?
   assignment: one identifier, one expression

-}


type alias Definition =
    List Constraint


type Constraint
    = Exactly Int String
    | AtLeast Int String


definitions : Dict String (List Constraint)
definitions =
    let
        one =
            Exactly 1

        many =
            AtLeast 0

        some =
            AtLeast 1
    in
    Dict.fromList <|
        [ ( "list", [ many "expression" ] )
        , ( "case", [ some "branch" ] )
        , ( "branch", [ one "pattern", one "expression" ] )
        , ( "assignment", [ one "name", one "expression" ] )
        , ( "hole", [] )
        ]


hole : Node
hole =
    Node 0 "" "hole"


holeT : Ast
holeT =
    Tree.singleton hole


newNode : String -> Maybe Ast
newNode name =
    Dict.get name definitions
        |> Maybe.map (generate name)


generate : String -> Definition -> Ast
generate name definition =
    let
        f constraint =
            case constraint of
                Exactly n _ ->
                    List.repeat n holeT

                AtLeast n _ ->
                    List.repeat n holeT
    in
    Tree.tree (Node 0 "" name) <|
        List.concatMap f definition



-- VIEW


view : Editor -> Element msg
view editor =
    let
        tree =
            Zipper.tree editor.zipper
    in
    viewTree (Tree.label tree).id (Zipper.tree <| Zipper.root editor.zipper)


viewTree : Int -> Ast -> Element msg
viewTree selected tree =
    let
        go =
            viewTree selected

        children =
            Tree.children tree

        label =
            Tree.label tree

        token =
            el [ alignTop ] << text

        keyword =
            el [ alignTop, Font.color (rgb255 136 89 168) ] << text

        line =
            row [ spacing 10 ]

        lines =
            column []

        indented =
            el [ paddingEach { left = 20, right = 0, top = 0, bottom = 0 } ]

        block =
            indented << lines

        attributes =
            if label.id == selected then
                [ Background.color (rgb255 138 217 235)

                --, explain Debug.todo
                ]

            else
                []
    in
    el attributes <|
        case ( label.tag, children ) of
            ( "type-arguments", _ ) ->
                line (List.map go children)

            ( "constructor", _ ) ->
                line (List.map go children)

            ( "constructors", c :: cs ) ->
                column [] <|
                    line [ token "=", go c ]
                        :: List.map (\e -> line [ token "|", go e ]) cs

            ( "generics", _ ) ->
                line (List.map go children)

            ( "type", [ name, generics, constructors ] ) ->
                column []
                    [ line [ keyword "type", go name, go generics ]
                    , indented <| go constructors
                    ]

            ( "type-alias", [ name, generics, type_ ] ) ->
                column []
                    [ line [ keyword "type alias", go name, go generics, token "=" ]
                    , indented <| go type_
                    ]

            ( "parenthesized-expression", [ e ] ) ->
                column [] <|
                    [ line [ token "(", go e ]
                    , token ")"
                    ]

            ( "declarations", declarations ) ->
                column [ spacing 40 ]
                    (List.map go declarations)

            ( "module-name", _ ) ->
                text label.text

            ( "module", [ moduleName, declarations ] ) ->
                column [ spacing 40 ] <|
                    [ line [ keyword "module", go moduleName, keyword "exposing", text "(..)" ]
                    , go declarations
                    ]

            ( "operator", [ l, r ] ) ->
                line [ go l, text label.text, go r ]

            ( "branch", [ l, r ] ) ->
                line [ go l, token "->", go r ]

            ( "branches", _ ) ->
                column [] <| List.map go children

            ( "case-of", [ name, branches ] ) ->
                column [] <|
                    [ line [ keyword "case", go name, keyword "of" ]
                    , indented <| go branches
                    ]

            ( "function-type", [ l, r ] ) ->
                line [ go l, token "->", go r ]

            ( "typed", [ name ] ) ->
                go name

            ( "signature", [ name, annotation ] ) ->
                line
                    [ el [ Font.color (rgb255 142 144 140) ] <| go name
                    , token ":"
                    , go annotation
                    ]

            ( "call", x :: xs ) ->
                column [] <|
                    [ go x
                    , block <| List.map go xs
                    ]

            ( "function-declaration", _ ) ->
                column [] (List.map go children)

            ( "arguments", _ ) ->
                line (List.map go children)

            ( "function-definition", [ name, arguments, expression ] ) ->
                column [] <|
                    [ line
                        [ el [ Font.color (rgb255 142 144 140) ] (go name)
                        , go arguments
                        , token "="
                        ]
                    , indented <| go expression
                    ]

            ( "list", x :: xs ) ->
                column [] <|
                    List.concat <|
                        [ [ line [ token "[", go x ] ]
                        , List.map (\e -> line [ token ",", go e ]) xs
                        , [ token "]" ]
                        ]

            ( "list", _ ) ->
                token "[]"

            ( "record-expression", x :: xs ) ->
                column [] <|
                    List.concat <|
                        [ [ line [ token "{", go x ] ]
                        , List.map (\e -> line [ token ",", go e ]) xs
                        , [ token "}" ]
                        ]

            ( "float", _ ) ->
                text label.text

            ( "int", _ ) ->
                el [ Font.color (rgb255 244 135 30) ] <| text label.text

            ( "name", _ ) ->
                text label.text

            ( "assignment", [ l, r ] ) ->
                line [ go l, token "=", go r ]

            ( "string", _ ) ->
                el [ Font.color (rgb255 113 140 0) ] <| text <| "\"" ++ label.text ++ "\""

            _ ->
                el [ explain Debug.todo ] <|
                    if not <| List.isEmpty children then
                        column []
                            [ text <| label.tag ++ ":"
                            , row []
                                [ text "  "
                                , column [] <| List.map (viewTree selected) children
                                ]
                            ]

                    else if label.tag == "hole" then
                        text "hole"

                    else
                        text (label.tag ++ ": " ++ label.text)



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

        leaf t v =
            Tree.singleton (Node 0 v t)

        file : File
        file =
            Processing.process Processing.init rawFile

        processLetDeclaration : LetDeclaration -> Tree Node
        processLetDeclaration letDeclaration =
            case letDeclaration of
                LetFunction { documentation, signature, declaration } ->
                    Debug.todo ""

                LetDestructuring mnode nnode ->
                    Debug.todo ""

        processRecordSetter ( name, expression ) =
            node "assignment" [ leaf "name" (N.value name), processExpression (N.value expression) ]

        processPattern : Pattern -> Tree Node
        processPattern pattern =
            case pattern of
                VarPattern s ->
                    leaf "name" s

                NamedPattern { moduleName, name } patterns ->
                    if not <| List.isEmpty moduleName && List.isEmpty patterns then
                        Debug.todo ""

                    else
                        leaf "name" name

                _ ->
                    Debug.todo ("Unsupported pattern: " ++ Debug.toString pattern)

        processExpression : Expression -> Tree Node
        processExpression e =
            case e of
                UnitExpr ->
                    leaf "unit" "()"

                PrefixOperator s ->
                    leaf "prefix-operator" s

                Operator s ->
                    leaf "operator" s

                Integer i ->
                    leaf "int" (String.fromInt i)

                ListExpr es ->
                    node "list" (List.map (processExpression << N.value) es)

                IfBlock e1 e2 e3 ->
                    node "if" (List.map (processExpression << N.value) [ e1, e2, e3 ])

                FunctionOrValue names s ->
                    case names of
                        [] ->
                            leaf "name" s

                        _ ->
                            -- TODO: This should be a qualified name of something
                            leaf "name" (String.join "." names ++ "." ++ s)

                Application nodes ->
                    node "call" (List.map (processExpression << N.value) nodes)

                OperatorApplication string infixDirection e1 e2 ->
                    case infixDirection of
                        Left ->
                            Tree.tree (Node 0 string "operator") [ processExpression (N.value e1), processExpression (N.value e2) ]

                        Right ->
                            node string [ processExpression (N.value e2), processExpression (N.value e1) ]

                        Non ->
                            Debug.todo ""

                Hex int ->
                    Debug.todo ""

                Floatable float ->
                    leaf "float" (String.fromFloat float)

                Negation exp ->
                    node "negation" [ processExpression (N.value exp) ]

                Literal string ->
                    leaf "string" string

                CharLiteral char ->
                    leaf "char" (String.fromChar char)

                TupledExpression nodes ->
                    node "tuple" (List.map (processExpression << N.value) nodes)

                ParenthesizedExpression n ->
                    node "parenthesized-expression" [ processExpression (N.value n) ]

                LetExpression { declarations, expression } ->
                    node "let" (List.map (N.value >> processLetDeclaration) declarations ++ [ (N.value >> processExpression) expression ])

                CaseExpression { expression, cases } ->
                    let
                        f ( pattern, exp ) =
                            node "branch" [ processPattern (N.value pattern), processExpression (N.value exp) ]
                    in
                    node "case-of" [ processExpression (N.value expression), node "branches" <| List.map f cases ]

                LambdaExpression { args, expression } ->
                    node "lambda" (List.map (N.value >> processPattern) args ++ [ processExpression (N.value expression) ])

                RecordExpr assignments ->
                    node "record-expression" <| List.map (N.value >> processRecordSetter) assignments

                RecordAccess exp name ->
                    node "record-access" [ N.value exp |> processExpression, leaf "name" (N.value name) ]

                RecordAccessFunction string ->
                    leaf "record-access-function" string

                RecordUpdateExpression name n2 ->
                    node "record-update" ([ leaf "name" (N.value name) ] ++ List.map (processRecordSetter << N.value) n2)

                GLSLExpression string ->
                    Debug.todo "GLSL expressions are not supported in this version."

        processFile : File -> Tree Node
        processFile f =
            let
                name =
                    (.moduleDefinition >> N.value >> Module.moduleName) f

                declarations =
                    file.declarations |> List.map (N.value >> processDecoration)
            in
            node "module" [ leaf "module-name" (String.join "." name), node "declarations" declarations ]

        processSignature : Signature -> Tree Node
        processSignature { name, typeAnnotation } =
            node "signature" [ leaf "name" (N.value name), processTypeAnnotation (N.value typeAnnotation) ]

        processTypeAnnotation : TypeAnnotation -> Tree Node
        processTypeAnnotation typeAnnotation =
            case typeAnnotation of
                GenericType name ->
                    Debug.todo ""

                Typed (N.Node _ ( moduleName, name )) annotations ->
                    let
                        x =
                            Debug.log "" annotations
                    in
                    node "typed" [ leaf "name" name ]

                Unit ->
                    leaf "unit" "()"

                Tupled annotations ->
                    Debug.todo ""

                Record recordDefinition ->
                    Debug.todo ""

                GenericRecord name recordDefinition ->
                    Debug.todo ""

                FunctionTypeAnnotation (N.Node _ annotationFrom) (N.Node _ annotationTo) ->
                    node "function-type" [ processTypeAnnotation annotationFrom, processTypeAnnotation annotationTo ]

        processConstructor : ValueConstructor -> Ast
        processConstructor { name, arguments } =
            node "constructor" [ leaf "name" (N.value name), node "type-arguments" <| List.map (processTypeAnnotation << N.value) arguments ]

        processDecoration : Declaration -> Tree Node
        processDecoration declaration =
            case declaration of
                AliasDeclaration { documentation, name, generics, typeAnnotation } ->
                    node "type-alias"
                        [ leaf "name" (N.value name)
                        , node "generics" (List.map (leaf "name" << N.value) generics)
                        , processTypeAnnotation (N.value typeAnnotation)
                        ]

                CustomTypeDeclaration { documentation, name, generics, constructors } ->
                    node "type"
                        [ leaf "name" (N.value name)
                        , node "generics" (List.map (leaf "name" << N.value) generics)
                        , node "constructors" (List.map (processConstructor << N.value) constructors)
                        ]

                FunctionDeclaration function ->
                    let
                        signature =
                            Maybe.map N.value function.signature

                        arguments =
                            List.map N.value <| .arguments <| N.value function.declaration

                        expression =
                            N.value <| .expression <| N.value function.declaration

                        name =
                            N.value <| .name <| N.value function.declaration
                    in
                    node "function-declaration" <|
                        case signature of
                            Nothing ->
                                [ node "function-definition" [ leaf "name" name, node "arguments" <| List.map processPattern arguments, processExpression expression ] ]

                            Just s ->
                                [ processSignature s
                                , node "function-definition" [ leaf "name" name, node "arguments" <| List.map processPattern arguments, processExpression expression ]
                                ]

                _ ->
                    Debug.todo ""
    in
    processFile file
