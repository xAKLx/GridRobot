module TRobot exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

import Grid exposing ( Grid )
import Plane2d exposing ( Cardinal(..), Position )
import Robot exposing ( Robot, updateRobotPosition )

suite : Test
suite =
  describe "The Robot Movement"
    [ describe "The Robot should be able to move"
        [ test "The Robot moves up" <|
            \_ ->
              let
                robot = Robot ( Position 3 3 ) N
                expectedResult = Robot ( Position 3 2 ) N
              in
              updateRobotPosition ( Grid 7 7 ) robot
                |> Expect.equal expectedResult

        , test "The Robot moves left" <|
            \_ ->
              let
                robot = Robot ( Position 5 1 ) W
                expectedResult = Robot ( Position 4 1 ) W
              in
              updateRobotPosition ( Grid 5 5 ) robot
                |> Expect.equal expectedResult
                
        , test "The Robot moves down" <|
            \_ ->
              let
                robot = Robot ( Position 5 1 ) S
                expectedResult = Robot ( Position 5 2 ) S
              in
              updateRobotPosition ( Grid 8 8 ) robot
                |> Expect.equal expectedResult
        ]
    , describe "The Robot shouldn't be able to move"
        [ test "The Robot moves up" <|
            \_ ->
              let
                robot = Robot ( Position 1 1 ) N
                expectedResult = Robot ( Position 1 1 ) N
              in
              updateRobotPosition ( Grid 7 7 ) robot
                |> Expect.equal expectedResult

        , test "The Robot moves right" <|
            \_ ->
              let
                robot = Robot ( Position 7 1 ) E
                expectedResult = Robot ( Position 7 1 ) E
              in
              updateRobotPosition ( Grid 7 7 ) robot
                |> Expect.equal expectedResult
        ]
    ]