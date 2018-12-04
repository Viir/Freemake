module Common exposing (flip, tuple2MapAll, tuple2ToList, withListTransformApplied)


withListTransformApplied : List (a -> a) -> a -> a
withListTransformApplied listTransform original =
    case listTransform |> List.head of
        Just transform ->
            withListTransformApplied (listTransform |> List.tail |> Maybe.withDefault []) (transform original)

        Nothing ->
            original


tuple2MapAll : (a -> b) -> ( a, a ) -> ( b, b )
tuple2MapAll map =
    Tuple.mapBoth map map


tuple2ToList : ( a, a ) -> List a
tuple2ToList ( e0, e1 ) =
    [ e0, e1 ]


flip : (a -> b -> c) -> (b -> a -> c)
flip func =
    \a b -> func b a
