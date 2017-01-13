module Transition exposing (..)

import State exposing (..)
import Grid exposing (..)
import Block exposing (..)
import Transformation exposing (..)


type alias Transition = State -> State


moveLeft : Transition
moveLeft state = 
    { state 
    | offset = state.offset @- (Point 1 0) 
    }


moveRight : Transition
moveRight state = 
    { state 
    | offset = state.offset @+ (Point 1 0) 
    }


moveDown : Transition
moveDown state = 
    { state 
    | offset = state.offset @+ (Point 0 1) 
    }


rotateBlock : Transition
rotateBlock state =
    { state 
    | block = rotate state.block
    , offset = state.offset @+ rotateOffset state.block 
    }


mergeBlock : Transition
mergeBlock state =
    { state 
    | grid = removeCompletedRows (merge state)
    , block = empty 
    }


applyIfValid : State -> Transition -> State
applyIfValid state transition = 
    case isValid (transition state) of
        True -> transition state
        False -> state