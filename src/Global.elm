module Global exposing ( Updater, rangeUntil, tupleList )

type alias Updater a = a -> a

{-| Creates list of integeres with values from 1 to the final number 
  rangeUntil 3 = [ 1, 2, 3]
-}
rangeUntil : Int -> List Int
rangeUntil finish = List.range 1 finish 

{-| Creates a list of tuples for each member of the list of values
  provided and the single value
  tupleList [ 1, 2, 3 ] 1 == [ (1, 1), (1, 2), (1, 3) ]  
-}
tupleList : List a -> b -> List ( b, a )
tupleList values value = List.map (\n -> ( value, n ) ) values
