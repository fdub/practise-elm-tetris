module Transformation exposing (rotate, rotateOffset, merge, isValid, removeCompletedRows)

import Array exposing (Array)

import Block exposing (..)
import State exposing (..)
import Grid exposing (..)


rotate : Block -> Block
rotate block =
    List.range 0 (block.grid.size.width - 1)
    |> List.map (\x ->
        List.range 0 (block.grid.size.height - 1)
        |> List.map (\y ->
            value block.grid (Point (block.grid.size.width - 1 - x) y)
        )
    )
    |> createGrid
    |> fromGrid


rotateOffset : Block -> Point
rotateOffset block =
    if block.grid.size == Size 4 1 then
        Point 1 -1
    else if block.grid.size == Size 1 4 then
        Point -1 1
    else
        Point 0 0


isValidBlockPosition : State -> Point -> Bool
isValidBlockPosition state point =
    point.x - state.offset.x >= 0 &&
    point.y - state.offset.y >= 0 &&
    point.x - state.offset.x < state.block.grid.size.width &&
    point.y - state.offset.y < state.block.grid.size.height    


mergedValue : State -> Point -> Int
mergedValue state point = 
    case isValidBlockPosition state point of
        False -> 
            value (state.grid) point
            
        True ->
            let 
                blockValue = value (state.block.grid) (point @- state.offset)
                gridValue = value (state.grid) point
            in
                if blockValue > 0 then
                    blockValue
                else
                    gridValue  


merge : State -> Grid
merge state =
    List.range 0 (state.grid.size.height - 1)
    |> List.map (\y ->
        List.range 0 (state.grid.size.width - 1)
        |> List.map (\x -> mergedValue state (Point x y))
    )
    |> createGrid


isMergedValueValid : State -> Point -> Bool
isMergedValueValid state point = 
    case isValidBlockPosition state point of
        False -> 
            True
            
        True ->
            value (state.grid) point == 0 || 
            value (state.block.grid) (point @- state.offset) == 0

isValid : State -> Bool
isValid state =
    let
        isMergeValid =
            List.range 0 (state.grid.size.height - 1)
            |> List.all (\y ->
                List.range 0 (state.grid.size.width - 1)
                |> List.all (\x -> isMergedValueValid state (Point x y))
            )
        isOffsetValid =
            state.offset.x + state.block.start.x >= 0 &&  
            state.offset.y + state.block.start.y >= 0 &&
            state.offset.x + state.block.end.x <= state.grid.size.width &&  
            state.offset.y + state.block.end.y <= state.grid.size.height
    in
        isMergeValid && isOffsetValid
    
removeCompletedRows : Grid -> Grid
removeCompletedRows grid =
    let 
        rows = 
            grid.rows
            |> Array.filter (\row ->
                row.pixels
                |> Array.toList
                |> List.any (\pixel -> pixel.value == 0)
            )
    in
        List.range 0 (grid.size.height - (Array.length rows) - 1)
        |> List.map (\_ -> 
            List.range 0 (grid.size.width - 1)
            |> List.map (\_ -> 0)
        )
        |> createGrid
        |> \newGrid -> { grid | rows = Array.append newGrid.rows rows }
    