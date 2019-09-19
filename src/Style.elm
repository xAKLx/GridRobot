module Style exposing ( BorderType(..), borderDirectionStyle, borderStyle, gridColumns, gridItemColumn, gridItemRow, gridRows )

import Html exposing ( Attribute )
import Html.Attributes exposing ( style )

type GridTemplateDimension = Row | Column
type BorderType = Solid

{-| Creates a css border style
  borderStyle 1 Solid "black" = style "boder" "1px solid black"
-}
borderStyle : Int -> BorderType -> String -> Attribute msg
borderStyle =
  borderDirectionStyle "border"

borderDirectionStyle : String -> Int -> BorderType -> String -> Attribute msg
borderDirectionStyle border size borderType color = 
  String.join " " [ intToPixel size, borderTypeToString borderType, color ] |> style border

{-| Gets the string representation of a BorderType
  borderTypeToString Solid == "solid"
-}
borderTypeToString : BorderType -> String
borderTypeToString borderType =
  case borderType of
    Solid -> "solid"

{-| Convert an integer into a String with pixel unit (px)

  intToPixel 50 == "50px"
-}
intToPixel : Int -> String
intToPixel number = String.fromInt number ++ "px"

gridItemColumn : Int -> Attribute msg
gridItemColumn position = String.fromInt position |> style "grid-column"

gridItemRow : Int -> Attribute msg
gridItemRow position = String.fromInt position |> style "grid-row"

gridColumns : List Int -> Attribute msg
gridColumns = gridTemplate Column

gridRows : List Int -> Attribute msg
gridRows = gridTemplate Row

{-| Creates a css grid template using the given list of integers as pixel units.
  gridTemplate Row [ 10, 20 ] == style "grid-template-row" "10px 20px"
-}
gridTemplate : GridTemplateDimension -> List Int -> Attribute msg
gridTemplate dimension sizes =
  sizes 
    |> List.map intToPixel
    |> String.join " "
    |> style 
      (  "grid-template-"  ++ case dimension of 
            Row -> "rows" 
            Column -> "columns" 
      )
