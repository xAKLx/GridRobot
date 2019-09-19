import Browser
import Html exposing ( Html, div )
import Html.Attributes exposing ( style )

import Control exposing ( renderControls )
import Grid exposing ( Grid, renderGrid )
import Plane2d exposing ( Cardinal(..), Position )
import Robot exposing ( Robot, renderRobot, updateRobotPosition )

main = Browser.sandbox { init = init, update = update, view = view }

-- MODEL

type alias Model = 
  { grid : Grid
  , robot: Robot
  }

init : Model
init = Model ( Grid 5 5 ) ( Robot ( Position 3 3 ) N )

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

-- VIEW

view : Model -> Html Msg
view model =
  div
    [ style "display" "flex" 
    , style "flex-direction" "column"
    , style "align-items" "center"
    ]
    [ renderGrid model.grid [ renderRobot model.robot ]
    , renderControls OnMove OnCardinalChange model.robot.facing
    ]
