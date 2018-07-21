module Visuals exposing (..)

type alias Float2 = (Float, Float)


svgTransformTranslate : Float2 -> String
svgTransformTranslate (x, y) =
  "translate(" ++ (x |> toString) ++ "," ++ (y |> toString) ++ ")"
