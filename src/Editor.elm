module Editor exposing (..)

import Action exposing (Action(..))
import Dict
import Element exposing (Element, alignBottom, column, el, rgb, rgb255, row, text)
import Element.Background as Background
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
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation(..))
import Maybe exposing (Maybe)
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
main =
  Browser.sandbox { init = init, update = update, view = view }
init : Model
init =
    0
update : Msg -> Model -> Model
update msg model =
  case msg of
    Increment ->
      model + 1

    Decrement ->
      model - 1


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
            , LastChild
            ]



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


zipperId =
    Zipper.label >> .id


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
        ]


hole =
    Node 0 "" "hole"


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
        children =
            Tree.children tree

        label =
            Tree.label tree

        attributes =
            if label.id == selected then
                [ Background.color (rgb255 138 217 235) ]

            else
                []
    in
    el attributes <|
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

        leaf v t =
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
            node "assignment" [ leaf (N.value name) "name", processExpression (N.value expression) ]

        processPattern : Pattern -> Tree Node
        processPattern pattern =
            case pattern of
                VarPattern s ->
                    leaf s "name"

                NamedPattern { moduleName, name } patterns ->
                    if not <| List.isEmpty moduleName && List.isEmpty patterns then
                        Debug.todo ""

                    else
                        leaf name "name"

                _ ->
                    Debug.todo ("Unsupported pattern: " ++ Debug.toString pattern)

        processExpression : Expression -> Tree Node
        processExpression e =
            case e of
                UnitExpr ->
                    leaf "()" "unit"

                PrefixOperator s ->
                    leaf s "prefix-operator"

                Operator s ->
                    leaf s "operator"

                Integer i ->
                    leaf (String.fromInt i) "int"

                ListExpr es ->
                    node "list" (List.map (processExpression << N.value) es)

                IfBlock e1 e2 e3 ->
                    node "if" (List.map (processExpression << N.value) [ e1, e2, e3 ])

                FunctionOrValue names s ->
                    case names of
                        [] ->
                            leaf s "name"

                        _ ->
                            -- TODO: This should be a qualified name of something
                            leaf (String.join "." names ++ "." ++ s) "name"

                Application nodes ->
                    node "call" (List.map (processExpression << N.value) nodes)

                OperatorApplication string infixDirection e1 e2 ->
                    case infixDirection of
                        Left ->
                            node string [ processExpression (N.value e1), processExpression (N.value e2) ]

                        Right ->
                            node string [ processExpression (N.value e2), processExpression (N.value e1) ]

                        Non ->
                            Debug.todo ""

                Hex int ->
                    Debug.todo ""

                Floatable float ->
                    leaf (String.fromFloat float) "float"

                Negation exp ->
                    node "negation" [ processExpression (N.value exp) ]

                Literal string ->
                    leaf "string" string

                CharLiteral char ->
                    leaf (String.fromChar char) "char"

                TupledExpression nodes ->
                    node "tuple" (List.map (processExpression << N.value) nodes)

                ParenthesizedExpression n ->
                    Debug.todo ""

                LetExpression { declarations, expression } ->
                    node "let" (List.map (N.value >> processLetDeclaration) declarations ++ [ (N.value >> processExpression) expression ])

                CaseExpression { expression, cases } ->
                    let
                        f ( pattern, exp ) =
                            node "branch" [ processPattern (N.value pattern), processExpression (N.value exp) ]
                    in
                    node "case-of" (processExpression (N.value expression) :: List.map f cases)

                LambdaExpression { args, expression } ->
                    node "lambda" (List.map (N.value >> processPattern) args ++ [ processExpression (N.value expression) ])

                RecordExpr assignments ->
                    node "record-expression" <| List.map (N.value >> processRecordSetter) assignments

                RecordAccess exp name ->
                    node "record-access" [ N.value exp |> processExpression, leaf (N.value name) "name" ]

                RecordAccessFunction string ->
                    leaf string "record-access-function"

                RecordUpdateExpression name n2 ->
                    node "record-update" ([ leaf (N.value name) "name" ] ++ List.map (processRecordSetter << N.value) n2)

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
            node "module" [ leaf (String.join "." name) "module-name", node "declarations" declarations ]

        processSignature : Signature -> Tree Node
        processSignature { name, typeAnnotation } =
            node "signature" [ leaf (N.value name) "name", processTypeAnnotation (N.value typeAnnotation) ]

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
                    node "typed" [ leaf name "name" ]

                Unit ->
                    leaf "()" "unit"

                Tupled annotations ->
                    Debug.todo ""

                Record recordDefinition ->
                    Debug.todo ""

                GenericRecord name recordDefinition ->
                    Debug.todo ""

                FunctionTypeAnnotation (N.Node _ annotationFrom) (N.Node _ annotationTo) ->
                    node "function-type" [ processTypeAnnotation annotationFrom, processTypeAnnotation annotationTo ]

        processDecoration : Declaration -> Tree Node
        processDecoration declaration =
            case declaration of
                FunctionDeclaration function ->
                    let
                        signature =
                            Maybe.map N.value function.signature

                        expression =
                            N.value <| .expression <| N.value function.declaration

                        name =
                            N.value <| .name <| N.value function.declaration
                    in
                    node "function-declaration" <|
                        case signature of
                            Nothing ->
                                [ node "assignment" [ leaf name "name", processExpression expression ] ]

                            Just s ->
                                [ node "signature" [ processSignature s ]
                                , node "assignment" [ leaf name "name", processExpression expression ]
                                ]

                _ ->
                    Debug.todo ""
    in
    processFile file
