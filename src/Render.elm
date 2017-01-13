module Render exposing (render)

import Array exposing (Array)
import Html exposing (..)
import Html.Attributes exposing (..)

import Grid exposing (..)
import Model exposing (..)


renderPixel : Pixel -> Html Msg
renderPixel pixel =
    div [ class ("block block-" ++ toString(pixel.value)) ] [ ]


renderRow : Row -> Html Msg
renderRow row =
    p [] (row.pixels |> Array.toList |> List.map renderPixel)


render : Grid -> Html Msg
render grid =
    div [ class ("field") ] (grid.rows |> Array.toList |> List.map renderRow)
