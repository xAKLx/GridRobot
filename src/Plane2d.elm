module Plane2d exposing ( Cardinal( .. ), Direction( .. ), Position, cardinalToDirection )

type Cardinal = N | E | S | W
type Direction = Horizontal | Vertical

type alias Position = 
  { x: Int
  , y: Int
  }


{-| Converts a cardinal (N | E | S | W) to a Direction (Horizontal | Vertical) -}
cardinalToDirection : Cardinal -> Direction
cardinalToDirection cardinal =
  case cardinal of
    N -> Vertical
    E -> Horizontal
    S -> Vertical
    W -> Horizontal
