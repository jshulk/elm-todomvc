port module Ports exposing (..)

import Json.Encode as JE


port persist : JE.Value -> Cmd msg
