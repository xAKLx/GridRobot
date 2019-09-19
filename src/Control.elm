module Control exposing ( renderControls )

import Html exposing ( Html, button, div, text )
import Html.Attributes exposing ( class, style )
import Html.Events exposing ( onClick )

import Figure exposing ( renderArrow )
import Plane2d exposing ( Cardinal(..) )
import Style exposing ( gridColumns, gridItemRow, gridItemColumn, gridRows )

{-| Renders the controls. The arrow for the given cardinal will be red instead of white -}
renderControls : msg -> (Cardinal -> msg) -> Cardinal -> Html msg
renderControls onMove onCardinalChange activeCardinal = 
  div 
    [ style "display" "grid"
    , style "width" "max-content"
    , gridRows <| List.repeat 3 60
    , gridColumns <| List.repeat 3 60
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
    , onClick <| onCardinalChange cardinal
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
