module Action exposing (..)


type Action
    = -- Navigation
      SelectParent
    | NextSibling
    | PreviousSibling
    | FirstChild
    | LastChild
      -- Editing
    | Delete
    | Lift -- (replace parent with focus node)
    | ReverseChildren
