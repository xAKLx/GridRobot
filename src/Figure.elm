module Figure exposing ( renderArrow )

import Html exposing ( Html, Attribute, div )
import Html.Attributes exposing ( style )

import Plane2d exposing ( Cardinal(..), Direction(..), cardinalToDirection )
import Style exposing ( BorderType(..), borderDirectionStyle )

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
