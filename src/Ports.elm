port module Ports exposing (..)

import Json.Encode as Encode


port playBell : String -> Cmd msg


port stopBells : () -> Cmd msg


port keepAwake : { session : Bool, screen : Bool } -> Cmd msg


port saveSettings : Encode.Value -> Cmd msg
