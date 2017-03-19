module Main exposing (..)

import Html exposing (..)
import Array exposing (Array)
import Random exposing (..)
import Time exposing (..)
import WebSocket

import Block exposing (..)
import Json exposing (..)
import Grid exposing (..)
import Model exposing (..)
import Ports exposing (..)
import Render exposing (..)
import State exposing (..)
import Transformation exposing (..)
import Transition exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : (Model, Cmd Msg)
init =
    (Model "" initState False 0 True, Cmd.none)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    if model.running then 
        case msg of
            KeyDown keyCode ->
                case keyCode of
                    37 -> 
                        ({ model | state = applyIfValid model.state moveLeft }, Cmd.none)
                    
                    38 ->
                        ({ model | state = applyIfValid model.state rotateBlock }, Cmd.none)
                    
                    39 ->
                        ({ model | state = applyIfValid model.state moveRight }, Cmd.none)
                    
                    40 ->
                        ({ model | falling = True }, Cmd.none)
                    
                    _ ->
                        (model, Cmd.none) 

            KeyUp keyCode ->
                case keyCode of 
                    40 ->
                        ({ model | falling = False }, Cmd.none)
                    _ ->
                        (model, Cmd.none)

            Tick time ->
                let 
                    modulo = 15
                    model_ = 
                        { model 
                        | frameCounter = 
                            if not model.falling then 
                                if model.frameCounter < 0 then 
                                    1
                                else 
                                    (model.frameCounter + 1) % modulo 
                            else if model.frameCounter < 0 then
                                model.frameCounter + 1
                            else 
                                0
                        }
                in 
                    if model_.frameCounter == 0 then 
                        if (isValid (moveDown model.state)) then
                            ({ model_ | state = moveDown model.state }, Cmd.none)
                        else
                            (model, Random.generate NewBlock (Random.int 0 6))
                    else
                        (model_, Cmd.none)   

            NewBlock index ->
                let
                    block = Array.get index blocks |> Maybe.withDefault empty
                    center = (model.state.grid.size.width - block.grid.size.width) // 2
                    state = mergeBlock model.state
                    model_ =
                        { model 
                        | state = 
                            { state 
                            | block = block
                            , offset = Point center 0 
                            }
                        , frameCounter = -5
                        }
                in
                    if (isValid model_.state) then
                        (model_, Cmd.none)
                    else
                        ({ model | running = False, message = "Game over!" }, Cmd.none)
            
            ReceiveMessage message ->
                case (decodeKeyValuePair message) of
                    Result.Ok (key,value) -> 
                        case key of 
                          "ADDROW" -> 
                            model ! [ Random.list 10 (Random.int 0 10) |> Random.generate RandomLine ]
                          _ -> 
                            model ! [ Cmd.none ]
                    Result.Err err ->
                        { model | message = err } ! [ Cmd.none ]
            
            RandomLine pixels ->
                case (validateRandomRow pixels) of
                    True ->
                        let 
                            addPixels state = { state | grid = addRandomRow pixels state.grid }
                        in
                            if (isValid (addPixels model.state)) then
                                { model | state = (addPixels model.state) } ! [ Cmd.none ]
                            else
                                { model | state = (addPixels (mergeBlock model.state)) } ! [ Random.generate NewBlock (Random.int 0 6) ]
                    False ->
                        model ! [ Random.list 10 (Random.int 0 10) |> Random.generate RandomLine ]
    else
        (model, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ keydown KeyDown
        , keyup KeyUp
        , Time.every (35 * Time.millisecond) Tick
        , WebSocket.listen "ws://localhost:8001/text" ReceiveMessage
        ]


view : Model -> Html Msg
view model =
    div []
        [ (merge model.state |> render)
        , h1 [] [ text model.message ]
        ]


initState : State
initState =
    State (emptyGrid (Size 10 20)) (blocks |> Array.get 0 |> Maybe.withDefault empty) (Point 8 0)
