port module Ports exposing (..)


port save : String -> Cmd msg


port load : () -> Cmd msg


port onLoad : (String -> msg) -> Sub msg
