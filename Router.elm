module Router exposing (..)

import TodoTypes as TT
import Navigation
import UrlParser exposing (Parser, (</>), format, oneOf, int, s, string)
import String


hashParser : Navigation.Location -> Result String TT.Visibility
hashParser location =
    UrlParser.parse identity pageParser (String.dropLeft 2 location.hash)


pageParser : Parser (TT.Visibility -> a) a
pageParser =
    oneOf
        [ format TT.All (s "all")
        , format TT.Completed (s "completed")
        , format TT.Active (s "active")
        ]
