module App exposing (..)

import Html exposing (Html, div, text, ul, li, input, label, button, Attribute, section, footer, p, header, h1, span, strong, a)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as Json
import Dom
import Task
import Navigation
import String
import UrlParser exposing (Parser, (</>), format, oneOf, int, s, string)


-- TODO
{-
   1. Store the todos to local storage
   2. Fetch the initial state from local storage
   3. Implement router
-}


type alias Todo =
    { id : Int
    , title : String
    , completed : Bool
    , isEditing : Bool
    }



-- Model


type alias Model =
    { todos : List Todo
    , field : String
    , uid : Int
    , visibility : Visibility
    }



-- init


init : Result String Visibility -> ( Model, Cmd Msg )
init result =
    urlUpdate result { todos = todosData, field = "", uid = 2, visibility = All }


todosData =
    [ { id = 1, title = "Something", completed = False, isEditing = False }
    , { id = 2, title = "Something else", completed = False, isEditing = False }
    ]


type Visibility
    = All
    | Active
    | Completed



-- Message


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


hashParser : Navigation.Location -> Result String Visibility
hashParser location =
    UrlParser.parse identity pageParser (String.dropLeft 2 location.hash)


pageParser : Parser (Visibility -> a) a
pageParser =
    oneOf
        [ format All (s "all")
        , format Completed (s "completed")
        , format Active (s "active")
        ]


urlUpdate : Result String Visibility -> Model -> ( Model, Cmd Msg )
urlUpdate result model =
    case Debug.log "result" result of
        Err _ ->
            ( model, Navigation.modifyUrl "#/all" )

        Ok visibility ->
            { model | visibility = visibility } ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UpdateField newValue ->
            ( { model | field = newValue }, Cmd.none )

        Toggle todo ->
            let
                newTodo =
                    { todo | completed = not todo.completed }

                updatedTodos =
                    updateList newTodo model.todos
            in
                { model | todos = updatedTodos } ! []

        ClearCompleted ->
            let
                incomplete =
                    \todo -> not todo.completed

                incompleteTodos =
                    List.filter incomplete model.todos
            in
                { model | todos = incompleteTodos } ! []

        Delete id ->
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

        Edit todo isEditing ->
            let
                newTodo =
                    { todo | isEditing = isEditing }

                newTodos =
                    updateList newTodo model.todos

                focus =
                    Dom.focus ("todo-" ++ (toString todo.id))
            in
                { model | todos = newTodos }
                    ! [ Task.perform (always NoOp) (always NoOp) focus ]

        UpdateEntry todo newTitle ->
            let
                newTodo =
                    { todo | title = newTitle }

                newTodos =
                    updateList newTodo model.todos
            in
                ( { model | todos = newTodos }, Cmd.none )

        Add ->
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

        ChangeVisibility visibility ->
            { model | visibility = visibility }
                ! []


updateList : Todo -> List Todo -> List Todo
updateList todo todos =
    List.map (updateListItem todo) todos


updateListItem : Todo -> Todo -> Todo
updateListItem newTodo oldTodo =
    if newTodo.id == oldTodo.id then
        newTodo
    else
        oldTodo


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        tagger code =
            if code == 13 then
                msg
            else
                NoOp
    in
        HE.on "keydown" (Json.map tagger HE.keyCode)


view : Model -> Html Msg
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
                , onEnter Add
                , HE.onInput UpdateField
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


todoListView : Visibility -> List Todo -> Html Msg
todoListView visibility todos =
    let
        filteredTodos =
            case visibility of
                All ->
                    todos

                Completed ->
                    List.filter (\todo -> todo.completed) todos

                Active ->
                    List.filter (\todo -> not todo.completed) todos
    in
        section [ HA.class "main" ]
            [ ul [ HA.id "todo-list" ]
                (List.map (todoView todoViewConfig) filteredTodos)
            ]


viewCount : List todo -> Html Msg
viewCount todos =
    span
        [ HA.id "todo-count" ]
        [ strong []
            [ text (toString (List.length todos)), text " todos left" ]
        ]


filtersView : Visibility -> Html Msg
filtersView visibility =
    ul
        [ HA.id "filters" ]
        [ filter "#/all" All visibility
        , filter "#/active" Active visibility
        , filter "#/completed" Completed visibility
        ]


filter : String -> Visibility -> Visibility -> Html Msg
filter uri visibility actualVisibility =
    li
        []
        [ a
            [ HA.href uri
            , HE.onClick (ChangeVisibility visibility)
            , HA.classList
                [ ( "selected"
                  , (visibility == actualVisibility)
                  )
                ]
            ]
            [ text (toString visibility) ]
        ]


viewControls : Visibility -> List Todo -> Html Msg
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
                , HE.onClick ClearCompleted
                ]
                [ text "Clear completed (1)" ]
            ]


type alias TodoViewConfig =
    { todoEditMessage : Todo -> Bool -> Msg
    , todoUpdateMessage : Todo -> String -> Msg
    }


todoViewConfig : TodoViewConfig
todoViewConfig =
    { todoEditMessage = Edit
    , todoUpdateMessage = UpdateEntry
    }


todoView : TodoViewConfig -> Todo -> Html Msg
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
                , HE.onClick (Toggle todo)
                ]
                []
            , label
                [ HA.class "view" ]
                [ text todo.title ]
            , button [ HA.class "destroy", HE.onClick (Delete todo.id) ] []
            , todoEditView config todo
            ]
        ]


todoEditView : TodoViewConfig -> Todo -> Html Msg
todoEditView config todo =
    input
        [ HA.class "edit"
        , HA.type' "text"
        , HA.id ("todo-" ++ toString todo.id)
        , HA.value todo.title
        , HE.onBlur (config.todoEditMessage todo False)
        , HE.onInput (config.todoUpdateMessage todo)
        , onEnter (config.todoEditMessage todo False)
        ]
        []


main : Program Never
main =
    Navigation.program (Navigation.makeParser hashParser)
        { init = init
        , view = view
        , update = update
        , urlUpdate = urlUpdate
        , subscriptions = \_ -> Sub.none
        }
