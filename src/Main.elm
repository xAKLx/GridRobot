import Browser
import Html exposing ( Attribute, Html, div, text )
import Html.Attributes exposing ( style )

main =
  Browser.sandbox { init = init, update = update, view = view }

-- MODEL

type alias Model = 
  { grid : Grid
  }

type alias Grid =
  { rows : Int
  , columns : Int
  }

type GridTemplateDimension = Row | Column
type BorderType = Solid

init : Model
init = Model ( Grid 5 5 )

-- UPDATE

type Msg 
  = Basic

update : Msg -> Model -> Model
update msg model =
  case msg of
    Basic -> model

-- VIEW

view : Model -> Html Msg
view model =
  renderGrid model.grid

renderGrid : Grid -> Html Msg
renderGrid grid =
  div 
    [ style "display" "grid"
    , List.repeat grid.rows 50 |> gridRows
    , List.repeat grid.columns 50 |> gridColumns 
    ]
    ( List.repeat ( grid.rows * grid.columns ) gridCell )

gridCell : Html Msg
gridCell =
  div
    [ borderStyle 1 Solid "black"
    ] []

{-| Creates a css border style
  borderStyle 1 Solid "black" = style "boder" "1px solid black"
-}
borderStyle : Int -> BorderType -> String -> Attribute msg
borderStyle size borderType color =
  String.join " " [ intToPixel size, borderTypeToString borderType, color ] |> style "border"

{-| Gets the string representation of a BorderType
  borderTypeToString Solid == "solid"
-}
borderTypeToString : BorderType -> String
borderTypeToString borderType =
  case borderType of
    Solid -> "solid"

gridColumns : List Int -> Attribute msg
gridColumns =
  gridTemplate Column

gridRows : List Int -> Attribute msg
gridRows =
  gridTemplate Row

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

{-| Convert an integer into a String with pixel unit (px)

  intToPixel 50 == "50px"
-}
intToPixel : Int -> String
intToPixel number =
  String.fromInt number ++ "px"