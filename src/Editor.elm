module Editor exposing (..)

import Action exposing (Action(..))
import Element exposing (Element)
import Tree
import Tree.Zipper as Zipper exposing (Zipper)



-- MODEL


type alias Editor =
    { nextId : Int
    , zipper : Zipper Node
    }


type alias Node =
    { id : Int
    , tag : String
    }


n =
    Node 0


example =
    steps [] <|
        Editor 0 <|
            Zipper.fromTree <|
                Tree.tree (n "1") [ Tree.singleton <| n "2", Tree.singleton <| n "3" ]



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



-- VIEW


view : { a | nextId : b, zipper : Zipper c } -> Element msg
view { nextId, zipper } =
    Element.text (Debug.toString zipper)



-- PARSER
