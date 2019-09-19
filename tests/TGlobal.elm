module TGlobal exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, intRange, list, string)
import Test exposing (..)

import Global exposing ( rangeUntil, tupleList )

suite : Test
suite =
  describe "Global Module"
    [ describe "tupleList"
        [ fuzz (intRange 0 43) "Generates correct list of tuples" <|
            \random ->
              tupleList [ 34, 56, random, 10, 90 ] random
              |> Expect.equal [ ( random, 34 ), ( random, 56 ), ( random, random ), ( random, 10 ), ( random, 90 ) ]
        ]

    , describe "rangeUntil"
        [ test "Generates correct range for 5" <|
            \_ ->
              rangeUntil 5
                |> Expect.equal [ 1, 2, 3, 4, 5 ]

        , test "Generates correct range for 3" <|
            \_ ->
              rangeUntil 3
                |> Expect.equal [ 1, 2, 3 ]

        , fuzz (intRange 1 43) "Generates correct number of values and correct edges" <|
            \random ->
              rangeUntil random
                |> Expect.all
                  [ \result -> List.length result |> Expect.equal random
                  , \result ->
                      case List.head result of 
                        Just value -> Expect.equal 1 value
                        Nothing -> Expect.fail "List mustn't be empty"
                 , \result ->
                      case List.reverse result |> List.head of 
                        Just value -> Expect.equal random value
                        Nothing -> Expect.fail "List mustn't be empty"
                  ]
        ]
    ]
