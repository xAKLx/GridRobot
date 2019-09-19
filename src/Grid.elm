module Grid exposing ( Grid, renderGrid )

import Html exposing ( Html, div )
import Html.Attributes exposing ( style )

import Global exposing ( rangeUntil, tupleList )
import Style exposing ( BorderType( .. ), borderStyle, gridColumns, gridItemColumn, gridItemRow, gridRows )

type alias Grid =
  { rows : Int
  , columns : Int
  }

renderGrid : Grid -> List ( Html msg ) -> Html msg
renderGrid grid content =
  div 
    [ style "display" "grid"
    , style "width" "max-content"
    , style "margin" "20px"
    , List.repeat grid.rows 50 |> gridRows
    , List.repeat grid.columns 50 |> gridColumns 
    ]
    ( content ++ renderGridCells grid )

renderGridCells : Grid -> List ( Html msg )
renderGridCells grid =
  rangeUntil grid.rows
    |> List.map ( tupleList <| rangeUntil grid.columns ) 
    |> List.concat
    |> List.map gridCell

gridCell : ( Int, Int ) -> Html msg
gridCell position =
  div
    [ borderStyle 1 Solid "black"
    , Tuple.first position |> gridItemRow
    , Tuple.second position |> gridItemColumn
    ] []
    