module Block exposing (Block, blocks, empty, fromGrid)

import Array exposing (Array)
import Grid exposing (..)


type alias Block = 
    { grid : Grid
    , start : Point
    , end : Point }


blockStart : Grid -> Point
blockStart grid =
    let
        startX = 
            List.range 0 (grid.size.width - 1) 
            |> List.filter (\x ->
                List.range 0 (grid.size.height - 1)
                |> List.any (\y -> value grid (Point x y) > 0)
            )
            |> List.minimum 
            |> Maybe.withDefault 0
        startY =
            List.range 0 (grid.size.height - 1) 
            |> List.filter (\y ->
                List.range 0 (grid.size.width - 1)
                |> List.any (\x -> value grid (Point x y) > 0)
            )
            |> List.minimum 
            |> Maybe.withDefault 0
    in
        Point startX startY 


blockEnd : Grid -> Point
blockEnd grid =
    let
        startX = 
            List.range 0 (grid.size.width - 1) 
            |> List.filter (\x ->
                List.range 0 (grid.size.height - 1)
                |> List.any (\y -> value grid (Point x y) > 0)
            )
            |> List.maximum
            |> Maybe.withDefault (grid.size.width - 1) 
            |> (+) 1
        startY =
            List.range 0 (grid.size.height - 1) 
            |> List.filter (\y ->
                List.range 0 (grid.size.width - 1)
                |> List.any (\x -> value grid (Point x y) > 0)
            )
            |> List.maximum 
            |> Maybe.withDefault (grid.size.height - 1)
            |> (+) 1
    in
        Point startX startY


fromGrid : Grid -> Block
fromGrid grid = 
    Block grid (blockStart grid) (blockEnd grid)


blocks : Array Block
blocks =
    [ [ [1, 1]
      , [1, 1]
      ]
    , [ [2] 
      , [2]
      , [2]
      , [2]
      ]
    , [ [0, 3, 3]
      , [3, 3, 0]
      ]
    , [ [4, 4, 0]
      , [0, 4, 4]
      ]
    , [ [0, 5, 0]
      , [0, 5, 0]
      , [5, 5, 0]
      ]    
    , [ [0, 6, 0]
      , [0, 6, 0]
      , [0, 6, 6] 
      ]
    , [ [0, 7, 0]
      , [7, 7, 7]
      , [0, 0, 0]
      ]
    ]
    |> List.map (createGrid >> fromGrid)
    |> Array.fromList


empty : Block
empty =
    Block ([] |> createGrid) (Point 0 0) (Point 0 0)
