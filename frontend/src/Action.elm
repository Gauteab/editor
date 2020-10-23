module Action exposing (..)


type Action
    = -- Navigation
      SelectParent
    | NextSibling
    | PreviousSibling
    | FirstChild
    | LastChild
    | Forward
    | Backward
      -- Editing
    | Delete
    | InsertText String
    | AddNode String
    | Lift -- (replace parent with focus node)
    | SwapRight
    | SwapLeft
    | ReverseChildren
