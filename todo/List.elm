module Todo.List exposing(Model)
import Todo.Item
type alias Model =
    {
        todos : List Todo.Item.Model
    }