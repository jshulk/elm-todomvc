module TodoTypes exposing (..)


type alias Todo =
    { id : Int
    , title : String
    , completed : Bool
    , isEditing : Bool
    }


type alias Model =
    { todos : List Todo
    , field : String
    , uid : Int
    , visibility : Visibility
    }


type Visibility
    = All
    | Active
    | Completed


type Msg
    = Delete Int
    | Toggle Todo
    | Edit Todo Bool
    | UpdateEntry Todo String
    | Add
    | UpdateField String
    | NoOp
    | ClearCompleted
    | ChangeVisibility Visibility


type alias TodoViewConfig =
    { todoEditMessage : Todo -> Bool -> Msg
    , todoUpdateMessage : Todo -> String -> Msg
    }
