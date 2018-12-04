module Common exposing (withListTransformApplied)


withListTransformApplied : List (a -> a) -> a -> a
withListTransformApplied listTransform original =
    case listTransform |> List.head of
        Just transform ->
            withListTransformApplied (listTransform |> List.tail |> Maybe.withDefault []) (transform original)

        Nothing ->
            original
