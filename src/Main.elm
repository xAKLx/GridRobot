import Browser
import Html exposing ( Attribute, Html, div, text, img )
import Html.Attributes exposing ( style, src )

main =
  Browser.sandbox { init = init, update = update, view = view }

-- MODEL

type alias Model = 
  { grid : Grid
  , robot: Robot
  }

type alias Grid =
  { rows : Int
  , columns : Int
  }

type alias Robot =
  { position: ( Int, Int )
  , facing: Cardinal
  }

type Cardinal = N | E | S | W

type GridTemplateDimension = Row | Column
type BorderType = Solid

init : Model
init = Model ( Grid 5 5 ) ( Robot ( 3, 3 ) N )

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
  renderGrid model.grid model.robot

renderGrid : Grid -> Robot -> Html Msg
renderGrid grid robot =
  div 
    [ style "display" "grid"
    , List.repeat grid.rows 50 |> gridRows
    , List.repeat grid.columns 50 |> gridColumns 
    ]
    ( [ renderRobot robot ] ++ renderGridCells grid )

renderRobot : Robot -> Html msg
renderRobot robot =
  img 
    [ src "assets/Robot.png"
    , Tuple.first robot.position |> gridItemRow
    , Tuple.second robot.position |> gridItemColumn
    , style "align-self" "center"
    , style "justify-self" "center"
    , style "width" "35px"
    ] []

renderGridCells : Grid -> List ( Html msg )
renderGridCells grid =
  rangeUntil grid.rows
    |> List.map ( tupleList <| rangeUntil grid.columns ) 
    |> List.concat
    |> List.map gridCell

{-| Creates a list of tuples for each member of the list of values
  provided and the single value
  tupleList [ 1, 2, 3 ] 1 == [ (1, 1), (1, 2), (1, 3) ]  
-}
tupleList : List a -> b -> List ( b, a )
tupleList values value =
  List.map (\n -> ( value, n ) ) values

{-| Creates list of integeres with values from 1 to the final number 
  rangeUntil 3 = [ 1, 2, 3]
-}
rangeUntil : Int -> List Int
rangeUntil finish = 
  List.range 1 finish 

gridCell : ( Int, Int ) -> Html msg
gridCell position =
  div
    [ borderStyle 1 Solid "black"
    , Tuple.first position |> gridItemRow
    , Tuple.second position |> gridItemColumn
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

gridItemColumn : Int -> Attribute msg
gridItemColumn position =
  String.fromInt position |> style "grid-column"

gridItemRow : Int -> Attribute msg
gridItemRow position =
  String.fromInt position |> style "grid-row"

{-| Convert an integer into a String with pixel unit (px)

  intToPixel 50 == "50px"
-}
intToPixel : Int -> String
intToPixel number =
  String.fromInt number ++ "px"