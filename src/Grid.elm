module Grid exposing (..)

import Array exposing (Array)
import Maybe exposing (..)


type alias Point = 
    { x : Int
    , y : Int 
    }

type alias Size =
    { width : Int
    , height : Int 
    }

type alias Pixel = 
    { value : Int 
    }

type alias Row = 
    { pixels : Array Pixel
    }

type alias Grid = 
    { rows : Array Row
    , size : Size 
    }


(@-) : Point -> Point -> Point
(@-) p1 p2 =
    Point (p1.x - p2.x) (p1.y - p2.y)


(@+) : Point -> Point -> Point
(@+) p1 p2 =
    Point (p1.x + p2.x) (p1.y + p2.y)


value : Grid -> Point -> Int
value grid point =
    let pixel = 
        grid.rows 
        |> Array.get point.y 
        |> andThen (\rows -> rows.pixels |> Array.get point.x)
    in
        case pixel of
            Nothing ->
                0
            
            Just p ->
                p.value


createGrid : List (List Int) -> Grid
createGrid data =
    let 
        rows = 
            data
            |> List.map (\row -> 
                row 
                |> List.map (\value -> Pixel value)
                |> Array.fromList
                |> \pixels -> Row pixels 
            )
            |> Array.fromList
        size =
            Size 
                (rows |> Array.get 0 |> andThen (\row -> Just (Array.length row.pixels)) |> Maybe.withDefault 0)
                (rows |> Array.length)
    in
        Grid rows size 


emptyGrid : Size -> Grid
emptyGrid size =
    List.range 0 (size.height - 1)
    |> List.map (\y -> List.range 0 (size.width - 1) |> List.map (\x -> 0))
    |> createGrid
