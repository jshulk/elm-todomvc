port module App exposing (..)

import Html exposing (Html, div, text, ul, li, input, label, button, Attribute, section, footer, p, header, h1, span, strong, a)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as JD
import Dom
import Task
import Navigation
import Json.Encode as JE
import TodoTypes as TT
import Encoders
import Decoders
import Router
import Utils
import Ports


emptyModel : TT.Model
emptyModel =
    { todos = []
    , field = ""
    , uid = 0
    , visibility = TT.All
    }



-- init


init : Maybe JE.Value -> Result String TT.Visibility -> ( TT.Model, Cmd TT.Msg )
init initialModel result =
    case initialModel of
        Just model ->
            let
                decodedModel =
                    case (JD.decodeValue Decoders.modelDecoder model) of
                        Err _ ->
                            emptyModel

                        Ok model ->
                            model
            in
                urlUpdate result decodedModel

        Nothing ->
            urlUpdate result emptyModel



-- Update


urlUpdate : Result String TT.Visibility -> TT.Model -> ( TT.Model, Cmd TT.Msg )
urlUpdate result model =
    case result of
        Err _ ->
            ( model, Navigation.modifyUrl "#/all" )

        Ok visibility ->
            { model | visibility = visibility } ! []


updateWithStorage : TT.Msg -> TT.Model -> ( TT.Model, Cmd TT.Msg )
updateWithStorage msg model =
    let
        ( newModel, cmds ) =
            update msg model
    in
        ( newModel, Cmd.batch ([ Ports.persist (Encoders.convertToJson newModel), cmds ]) )


update : TT.Msg -> TT.Model -> ( TT.Model, Cmd TT.Msg )
update msg model =
    case msg of
        TT.NoOp ->
            ( model, Cmd.none )

        TT.UpdateField newValue ->
            ( { model | field = newValue }, Cmd.none )

        TT.Toggle todo ->
            let
                newTodo =
                    { todo | completed = not todo.completed }

                updatedTodos =
                    Utils.updateList newTodo model.todos
            in
                { model | todos = updatedTodos } ! []

        TT.ClearCompleted ->
            let
                incomplete =
                    \todo -> not todo.completed

                incompleteTodos =
                    List.filter incomplete model.todos
            in
                { model | todos = incompleteTodos } ! []

        TT.Delete id ->
            let
                todoFilter =
                    \todo ->
                        if todo.id == id then
                            False
                        else
                            True

                filteredTodos =
                    List.filter todoFilter model.todos
            in
                ( { model | todos = filteredTodos }, Cmd.none )

        TT.Edit todo isEditing ->
            let
                newTodo =
                    { todo | isEditing = isEditing }

                newTodos =
                    Utils.updateList newTodo model.todos

                focus =
                    Dom.focus ("todo-" ++ (toString todo.id))
            in
                { model | todos = newTodos }
                    ! [ Task.perform (always TT.NoOp) (always TT.NoOp) focus ]

        TT.UpdateEntry todo newTitle ->
            let
                newTodo =
                    { todo | title = newTitle }

                newTodos =
                    Utils.updateList newTodo model.todos
            in
                ( { model | todos = newTodos }, Cmd.none )

        TT.Add ->
            let
                newTodo =
                    { id = model.uid + 1
                    , title = model.field
                    , completed = False
                    , isEditing = False
                    }

                updatedTodos =
                    newTodo :: model.todos
            in
                ( { model | todos = updatedTodos, field = "", uid = model.uid + 1 }, Cmd.none )

        TT.ChangeVisibility visibility ->
            { model | visibility = visibility }
                ! []



-- View


view : TT.Model -> Html TT.Msg
view model =
    section
        [ HA.id "todoapp" ]
        [ header
            [ HA.id "header" ]
            [ h1
                []
                [ text "todos" ]
            , input
                [ HA.type' "text"
                , HA.id "new-todo"
                , Utils.onEnter TT.Add
                , HE.onInput TT.UpdateField
                , HA.type' "text"
                , HA.value model.field
                , HA.class "new-todo"
                ]
                []
            ]
        , todoListView model.visibility model.todos
        , viewControls model.visibility model.todos
        , footer
            [ HA.id "info" ]
            [ p
                []
                [ text "Double-click to edit a todo" ]
            ]
        ]


todoListView : TT.Visibility -> List TT.Todo -> Html TT.Msg
todoListView visibility todos =
    let
        filteredTodos =
            case visibility of
                TT.All ->
                    todos

                TT.Completed ->
                    List.filter (\todo -> todo.completed) todos

                TT.Active ->
                    List.filter (\todo -> not todo.completed) todos
    in
        section [ HA.class "main" ]
            [ ul [ HA.id "todo-list" ]
                (List.map (todoView todoViewConfig) filteredTodos)
            ]


viewCount : List todo -> Html TT.Msg
viewCount todos =
    span
        [ HA.id "todo-count" ]
        [ strong []
            [ text (toString (List.length todos)), text " todos left" ]
        ]


filtersView : TT.Visibility -> Html TT.Msg
filtersView visibility =
    ul
        [ HA.id "filters" ]
        [ filter "#/all" TT.All visibility
        , filter "#/active" TT.Active visibility
        , filter "#/completed" TT.Completed visibility
        ]


filter : String -> TT.Visibility -> TT.Visibility -> Html TT.Msg
filter uri visibility actualVisibility =
    li
        []
        [ a
            [ HA.href uri
            , HE.onClick (TT.ChangeVisibility visibility)
            , HA.classList
                [ ( "selected"
                  , (visibility == actualVisibility)
                  )
                ]
            ]
            [ text (toString visibility) ]
        ]


viewControls : TT.Visibility -> List TT.Todo -> Html TT.Msg
viewControls visibility todos =
    let
        pendingTodos =
            List.filter (\todo -> not todo.completed) todos
    in
        footer
            [ HA.id "footer" ]
            [ viewCount pendingTodos
            , filtersView visibility
            , button
                [ HA.id "clear-completed"
                , HE.onClick TT.ClearCompleted
                ]
                [ text "Clear completed (1)" ]
            ]


todoViewConfig : TT.TodoViewConfig
todoViewConfig =
    { todoEditMessage = TT.Edit
    , todoUpdateMessage = TT.UpdateEntry
    }


todoView : TT.TodoViewConfig -> TT.Todo -> Html TT.Msg
todoView config todo =
    li
        [ HE.onDoubleClick (config.todoEditMessage todo True)
        , HA.id (toString todo.id)
        , HA.classList
            [ ( "editing", todo.isEditing )
            , ( "completed", todo.completed )
            ]
        ]
        [ div []
            [ input
                [ HA.class "toggle"
                , HA.type' "checkbox"
                , HA.checked todo.completed
                , HE.onClick (TT.Toggle todo)
                ]
                []
            , label
                [ HA.class "view" ]
                [ text todo.title ]
            , button [ HA.class "destroy", HE.onClick (TT.Delete todo.id) ] []
            , todoEditView config todo
            ]
        ]


todoEditView : TT.TodoViewConfig -> TT.Todo -> Html TT.Msg
todoEditView config todo =
    input
        [ HA.class "edit"
        , HA.type' "text"
        , HA.id ("todo-" ++ toString todo.id)
        , HA.value todo.title
        , HE.onBlur (config.todoEditMessage todo False)
        , HE.onInput (config.todoUpdateMessage todo)
        , Utils.onEnter (config.todoEditMessage todo False)
        ]
        []


main : Program (Maybe JE.Value)
main =
    Navigation.programWithFlags (Navigation.makeParser Router.hashParser)
        { init = init
        , view = view
        , update = updateWithStorage
        , urlUpdate = urlUpdate
        , subscriptions = \_ -> Sub.none
        }
