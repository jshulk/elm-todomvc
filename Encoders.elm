module Encoders exposing (..)

import TodoTypes
import Json.Encode as JE


convertToJson : TodoTypes.Model -> JE.Value
convertToJson model =
    JE.object
        [ ( "todos", JE.list (List.map todoToValue model.todos) )
        , ( "field", JE.string model.field )
        , ( "uid", JE.int model.uid )
        , ( "visibility", JE.string (toString model.visibility) )
        ]


todoToValue : TodoTypes.Todo -> JE.Value
todoToValue todo =
    JE.object
        [ ( "id", JE.int todo.id )
        , ( "title", JE.string todo.title )
        , ( "completed", JE.bool todo.completed )
        , ( "isEditing", JE.bool todo.isEditing )
        ]
