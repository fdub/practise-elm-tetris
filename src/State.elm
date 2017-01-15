module State exposing (State)

import Grid exposing (..)
import Block exposing (..)


type alias State = 
    { grid : Grid
    , block : Block
    , offset : Point
    , score : Int }