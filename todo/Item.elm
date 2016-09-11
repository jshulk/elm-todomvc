module Todo.Item exposing (Model)

-- Model


type alias Model =
    { title : String
    , id : Int
    , completed : Bool
    , editing : Bool
    }



-- Msg


type Msg
    = Delete
    | Toggle
    | Edit
    | Update String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Delete ->
            ( model, Cmd.none )

        Toggle ->
            ( { model | completed = not model.completed }, Cmd.none )

        Edit ->
            ( { model | editing = True }, Cmd.none )

        Update newTitle ->
            ( { model | title = newTitle }, Cmd.none )
