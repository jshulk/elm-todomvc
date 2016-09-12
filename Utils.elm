module Utils exposing (..)

import Html exposing (Attribute)
import Html.Events as HE
import TodoTypes as TT
import Json.Decode as JD


-- Utils


updateList : TT.Todo -> List TT.Todo -> List TT.Todo
updateList todo todos =
    List.map (updateListItem todo) todos


updateListItem : TT.Todo -> TT.Todo -> TT.Todo
updateListItem newTodo oldTodo =
    if newTodo.id == oldTodo.id then
        newTodo
    else
        oldTodo



-- Custom Event


onEnter : TT.Msg -> Attribute TT.Msg
onEnter msg =
    let
        tagger code =
            if code == 13 then
                msg
            else
                TT.NoOp
    in
        HE.on "keydown" (JD.map tagger HE.keyCode)
