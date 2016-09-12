module Decoders exposing (..)

import TodoTypes
import Json.Decode as JD exposing ((:=), andThen)


-- Decoders


modelDecoder : JD.Decoder TodoTypes.Model
modelDecoder =
    JD.object4 TodoTypes.Model
        ("todos" := JD.list taskDecoder)
        ("field" := JD.string)
        ("uid" := JD.int)
        ("visibility" := JD.string `andThen` visibilityDecoder)


taskDecoder : JD.Decoder TodoTypes.Todo
taskDecoder =
    JD.object4 TodoTypes.Todo
        ("id" := JD.int)
        ("title" := JD.string)
        ("completed" := JD.bool)
        ("isEditing" := JD.bool)


visibilityDecoder : String -> JD.Decoder TodoTypes.Visibility
visibilityDecoder str =
    case str of
        "All" ->
            JD.succeed TodoTypes.All

        "Active" ->
            JD.succeed TodoTypes.Active

        "Completed" ->
            JD.succeed TodoTypes.Completed

        _ ->
            JD.succeed TodoTypes.All
