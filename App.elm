port module App exposing (..)

import Html exposing (Html, div, text, ul, li, input, label, button, Attribute, section, footer, p, header, h1, span, strong, a)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as JD exposing ((:=), andThen)
import Dom
import Task
import Navigation
import String
import UrlParser exposing (Parser, (</>), format, oneOf, int, s, string)
import Json.Encode as JE


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


init : Maybe JE.Value -> Result String Visibility -> ( Model, Cmd Msg )
init initialModel result =
    case initialModel of
        Just model ->
            let
                decodedModel =
                    case (JD.decodeValue modelDecoder model) of
                        Err _ ->
                            emptyModel

                        Ok model ->
                            model
            in
                urlUpdate result decodedModel

        Nothing ->
            urlUpdate result emptyModel


modelDecoder : JD.Decoder Model
modelDecoder =
    JD.object4 Model
        ("todos" := JD.list taskDecoder)
        ("field" := JD.string)
        ("uid" := JD.int)
        ("visibility" := JD.string `andThen` visibilityDecoder)


taskDecoder : JD.Decoder Todo
taskDecoder =
    JD.object4 Todo
        ("id" := JD.int)
        ("title" := JD.string)
        ("completed" := JD.bool)
        ("isEditing" := JD.bool)


visibilityDecoder str =
    case str of
        "All" ->
            JD.succeed All

        "Active" ->
            JD.succeed Active

        "Completed" ->
            JD.succeed Completed

        _ ->
            JD.succeed All


emptyModel =
    { todos = []
    , field = ""
    , uid = 0
    , visibility = All
    }


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


port persist : JE.Value -> Cmd msg


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
    case result of
        Err _ ->
            ( model, Navigation.modifyUrl "#/all" )

        Ok visibility ->
            { model | visibility = visibility } ! []


updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
        ( newModel, cmds ) =
            update msg model
    in
        ( newModel, Cmd.batch ([ persist (convertToJson newModel), cmds ]) )


convertToJson : Model -> JE.Value
convertToJson model =
    JE.object
        [ ( "todos", JE.list (List.map todoToValue model.todos) )
        , ( "field", JE.string model.field )
        , ( "uid", JE.int model.uid )
        , ( "visibility", JE.string (toString model.visibility) )
        ]


todoToValue : Todo -> JE.Value
todoToValue todo =
    JE.object
        [ ( "id", JE.int todo.id )
        , ( "title", JE.string todo.title )
        , ( "completed", JE.bool todo.completed )
        , ( "isEditing", JE.bool todo.isEditing )
        ]


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
        HE.on "keydown" (JD.map tagger HE.keyCode)


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


main : Program (Maybe JE.Value)
main =
    Navigation.programWithFlags (Navigation.makeParser hashParser)
        { init = init
        , view = view
        , update = updateWithStorage
        , urlUpdate = urlUpdate
        , subscriptions = \_ -> Sub.none
        }
