import Browser
import Html exposing ( Attribute, Html, div, text, img, button )
import Html.Attributes exposing ( style, src, class )
import Html.Events exposing ( onClick )

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
type Direction = Horizontal | Vertical

type GridTemplateDimension = Row | Column
type BorderType = Solid

init : Model
init = Model ( Grid 5 5 ) ( Robot ( 3, 3 ) N )

-- UPDATE

type Msg 
  = OnMove
  | OnCardinalChange Cardinal

update : Msg -> Model -> Model
update msg model =
  case msg of
    OnMove ->
      { model | robot = updateRobotPosition model.grid model.robot }

    OnCardinalChange cardinal ->
        let
          robot = model.robot
        in
        { model | robot = { robot | facing = cardinal } }

type alias Updater a = a -> a

{-| Moves the robot in the direction is facing if the destination is inside the grid. -}
updateRobotPosition : Grid -> Updater Robot
updateRobotPosition grid robot =
  let
    facing = robot.facing
    position = robot.position
    positionX = Tuple.first position
    positionY = Tuple.second position
  in
  case facing of
    N -> { robot | position = ( positionX, if positionY == 1 then 1 else positionY - 1 ) }
    E -> { robot | position = ( if positionX == grid.columns then positionX else positionX + 1, positionY ) }
    S -> { robot | position = ( positionX, if positionY == grid.rows then positionY else positionY + 1 ) }
    W -> { robot | position = ( if positionX == 1 then 1 else positionX - 1, positionY ) }

-- VIEW

view : Model -> Html Msg
view model =
  div
    [ style "display" "flex" 
    , style "flex-direction" "column"
    , style "align-items" "center"
    ]
    [ renderGrid model.grid model.robot
    , renderControls OnMove OnCardinalChange model.robot.facing
    ]

renderGrid : Grid -> Robot -> Html Msg
renderGrid grid robot =
  div 
    [ style "display" "grid"
    , style "width" "max-content"
    , style "margin" "20px"
    , List.repeat grid.rows 50 |> gridRows
    , List.repeat grid.columns 50 |> gridColumns 
    ]
    ( [ renderRobot robot ] ++ renderGridCells grid )

renderRobot : Robot -> Html msg
renderRobot robot =
  img 
    [ src "assets/Robot.png"
    , Tuple.second robot.position |> gridItemRow
    , Tuple.first robot.position |> gridItemColumn
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

{-| Renders the controls. The arrow for the given cardinal will be red instead of white -}
renderControls : msg -> (Cardinal -> msg) -> Cardinal -> Html msg
renderControls onMove onCardinalChange activeCardinal = 
  div 
    [ style "display" "grid"
    , style "width" "max-content"
    , List.repeat 3 60 |> gridRows
    , List.repeat 3 60 |> gridColumns 
    ]
    ( button 
        [ class "controlButton"
        , gridItemColumn 2
        , gridItemRow 2
        , onClick onMove
        ] [ text "MOVE" ]
    :: List.map 
        (\n -> renderCardinalChangeButton n ( n == activeCardinal ) onCardinalChange ) 
        [ N, E, S, W ] 
    )

renderCardinalChangeButton : Cardinal -> Bool -> ( Cardinal -> msg ) -> Html msg
renderCardinalChangeButton cardinal active onCardinalChange =
  button 
    [ class "controlButton"
    , gridItemColumn <| cardinalToColumn cardinal
    , gridItemRow <| cardinalToRow cardinal
    , onCardinalChange cardinal |> onClick
    ] [ renderArrow cardinal ( if active then "red" else "white" ) 15 ]

{-| Returns the column position in a 3x3 grid for the given cardinal (being 1 the first position and 3 the last one) -}
cardinalToColumn : Cardinal -> Int
cardinalToColumn cardinal = 
  case cardinal of
    N -> 2
    E -> 3
    S -> 2
    W -> 1

{-| Returns the row position in a 3x3 grid for the given cardinal (being 1 the first position and 3 the last one) -}
cardinalToRow : Cardinal -> Int
cardinalToRow cardinal = 
  case cardinal of
    N -> 1
    E -> 2
    S -> 3
    W -> 2

{-| Renders an arrow facing a direction depending on the cardinal provided -}
renderArrow : Cardinal -> String -> Int -> Html msg
renderArrow cardinal color size =
  div
    ( [ borderDirectionStyle ( cardinalBorder cardinal ) size Solid color ] 
    ++ commonCardinalArrowAttributes size ( cardinalToDirection cardinal )
    ) []

cardinalBorder : Cardinal -> String
cardinalBorder cardinal =
  "border-" ++ case cardinal of
    N -> "bottom"
    E -> "left"
    S -> "top"
    W -> "right"

{-| Converts a cardinal (N | E | S | W) to a Direction (Horizontal | Vertical) -}
cardinalToDirection : Cardinal -> Direction
cardinalToDirection cardinal =
  case cardinal of
    N -> Vertical
    E -> Horizontal
    S -> Vertical
    W -> Horizontal

commonCardinalArrowAttributes : Int -> Direction -> List (Attribute msg)
commonCardinalArrowAttributes size direction =
  case direction of
    Horizontal -> commonArrowHorizontalAttributes size ++ commonArrowAttributes
    Vertical -> commonArrowVerticalAttributes size ++ commonArrowAttributes

commonArrowVerticalAttributes : Int -> List (Attribute msg)
commonArrowVerticalAttributes size =
  [ "border-left", "border-right" ]
    |> List.map (\n -> borderDirectionStyle n size Solid "transparent")

commonArrowHorizontalAttributes : Int -> List (Attribute msg)
commonArrowHorizontalAttributes size =
  [ "border-top", "border-bottom" ]
    |> List.map (\n -> borderDirectionStyle n size Solid "transparent")

commonArrowAttributes : List (Attribute msg)
commonArrowAttributes = 
  [ style "width" "0" , style "height" "0" ]
