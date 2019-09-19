module Robot exposing ( Robot, updateRobotPosition, renderRobot )

import Html exposing ( Html, img )
import Html.Attributes exposing ( src, style )

import Global exposing ( Updater )
import Grid exposing ( Grid )
import Plane2d exposing ( Cardinal(..) )
import Style exposing ( gridItemColumn, gridItemRow )

type alias Robot =
  { position: ( Int, Int )
  , facing: Cardinal
  }

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
