module Visuals exposing (..)

import Point2d exposing (Point2d)
import Svg
import Svg.Attributes as SA


type SvgPathConnectionFromPreviousElement = MoveTo | LineTo

type alias Float2 = (Float, Float)


svgTransformTranslate : Float2 -> String
svgTransformTranslate (x, y) =
  "translate(" ++ (x |> toString) ++ "," ++ (y |> toString) ++ ")"

svgPolylineWithStroke : (String, Float) -> List Point2d -> Svg.Svg event
svgPolylineWithStroke (stroke, strokeWidth) points =
    let
        pathData = points |> svgPathDataFromPolylineListPoint MoveTo
    in
        Svg.path [ SA.d pathData, SA.stroke stroke, SA.strokeWidth (strokeWidth |> toString), SA.fill "none" ] []

svgPathDataFromPolylineListPoint : SvgPathConnectionFromPreviousElement -> List Point2d -> String
svgPathDataFromPolylineListPoint connectionFromPreviousSubpath polygonListPoint =
  let
    pathDataFromPoint =
      Point2d.coordinates >> (\(x, y) -> [x, y]) >> List.map toString >> String.join " "
  in
    case (polygonListPoint |> List.head, polygonListPoint |> List.tail) of
    (Just head, Just tail) ->
      (connectionFromPreviousSubpath |> svgPathConnectionStringFromType) ++ " " ++ (pathDataFromPoint head) ++ " " ++
      ((tail |> List.map (\point -> "L " ++ (pathDataFromPoint point)) |> String.join " "))
    _ -> ""

svgPathConnectionStringFromType : SvgPathConnectionFromPreviousElement -> String
svgPathConnectionStringFromType connectionFromPreviousElement =
  case connectionFromPreviousElement of
  MoveTo -> "M"
  LineTo -> "L"
