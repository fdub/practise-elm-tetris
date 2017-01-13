port module Ports exposing (..)

port keydown : (Int -> msg) -> Sub msg
port keyup : (Int -> msg) -> Sub msg
