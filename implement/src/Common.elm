module Common exposing (flip, tuple2MapAll, tuple2ToList)


tuple2MapAll : (a -> b) -> ( a, a ) -> ( b, b )
tuple2MapAll map =
    Tuple.mapBoth map map


tuple2ToList : ( a, a ) -> List a
tuple2ToList ( e0, e1 ) =
    [ e0, e1 ]


flip : (a -> b -> c) -> (b -> a -> c)
flip func =
    \a b -> func b a
