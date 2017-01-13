module Model exposing (..)

import State exposing (..)
import Time exposing (..)


type alias Model =
    { message : String
    , state : State
    , falling : Bool
    , frameCounter : Int
    , running : Bool
    }


type Msg
    = KeyDown Int
    | KeyUp Int
    | Tick Time
    | NewBlock Int
