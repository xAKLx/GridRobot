module Robot exposing ( Robot, updateRobotPosition, renderRobot )

import Html exposing ( Html, img )
import Html.Attributes exposing ( src, style )

import Global exposing ( Updater )
import Grid exposing ( Grid )
import Plane2d exposing ( Cardinal(..), Position )
import Style exposing ( gridItemColumn, gridItemRow )

type alias Robot =
  { position: Position
  , facing: Cardinal
  }

{-| Moves the robot in the direction is facing if the destination is inside the grid. -}
updateRobotPosition : Grid -> Updater Robot
updateRobotPosition grid robot =
  let
    facing = robot.facing
    position = robot.position
    positionX = position.x
    positionY = position.y
  in
  case facing of
    N -> { robot | position = Position positionX <| if positionY == 1 then 1 else positionY - 1 }
    E -> { robot | position = Position  ( if positionX == grid.columns then positionX else positionX + 1 ) positionY }
    S -> { robot | position = Position  positionX <| if positionY == grid.rows then positionY else positionY + 1 }
    W -> { robot | position = Position  ( if positionX == 1 then 1 else positionX - 1 ) positionY }

renderRobot : Robot -> Html msg
renderRobot robot =
  img 
    [ src "assets/Robot.png"
    , robot.position.y |> gridItemRow
    , robot.position.x |> gridItemColumn
    , style "align-self" "center"
    , style "justify-self" "center"
    , style "width" "35px"
    ] []
