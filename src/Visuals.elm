module Visuals exposing
    ( Float2
    , HtmlStyle
    , SvgPathConnectionFromPreviousElement(..)
    , button
    , cssFontFamily
    , htmlStyleFromList
    , svgLineSegmentWithStroke
    , svgPathConnectionStringFromType
    , svgPathDataFromPolylineListPoint
    , svgPolylineWithStroke
    , svgTransformTranslate
    , svgViewBoxFromBoundingBox
    )

import BoundingBox2d exposing (BoundingBox2d)
import Common exposing (..)
import Html
import Html.Attributes as HA
import LineSegment2d exposing (LineSegment2d)
import Point2d exposing (Point2d)
import Svg
import Svg.Attributes as SA


type SvgPathConnectionFromPreviousElement
    = MoveTo
    | LineTo


type alias Float2 =
    ( Float, Float )


type alias HtmlStyle a =
    List (Html.Attribute a)


htmlStyleFromList : List ( String, String ) -> List (Html.Attribute a)
htmlStyleFromList =
    List.map (\( attribute, value ) -> HA.style attribute value)


cssFontFamily : String
cssFontFamily =
    "'Segoe UI', Tahoma, Geneva, Verdana, sans-serif"


svgTransformTranslate : Float2 -> String
svgTransformTranslate ( x, y ) =
    "translate(" ++ (x |> String.fromFloat) ++ "," ++ (y |> String.fromFloat) ++ ")"


svgLineSegmentWithStroke : ( String, Float ) -> LineSegment2d -> Svg.Svg event
svgLineSegmentWithStroke stroke lineSegment =
    svgPolylineWithStroke stroke (lineSegment |> LineSegment2d.endpoints |> tuple2ToList)


svgPolylineWithStroke : ( String, Float ) -> List Point2d -> Svg.Svg event
svgPolylineWithStroke ( stroke, strokeWidth ) points =
    let
        pathData =
            points |> svgPathDataFromPolylineListPoint MoveTo
    in
    Svg.path [ SA.d pathData, SA.stroke stroke, SA.strokeWidth (strokeWidth |> String.fromFloat), SA.fill "none" ] []


svgPathDataFromPolylineListPoint : SvgPathConnectionFromPreviousElement -> List Point2d -> String
svgPathDataFromPolylineListPoint connectionFromPreviousSubpath polygonListPoint =
    let
        pathDataFromPoint =
            Point2d.coordinates >> (\( x, y ) -> [ x, y ]) >> List.map String.fromFloat >> String.join " "
    in
    case ( polygonListPoint |> List.head, polygonListPoint |> List.tail ) of
        ( Just head, Just tail ) ->
            (connectionFromPreviousSubpath |> svgPathConnectionStringFromType)
                ++ " "
                ++ pathDataFromPoint head
                ++ " "
                ++ (tail |> List.map (\point -> "L " ++ pathDataFromPoint point) |> String.join " ")

        _ ->
            ""


svgPathConnectionStringFromType : SvgPathConnectionFromPreviousElement -> String
svgPathConnectionStringFromType connectionFromPreviousElement =
    case connectionFromPreviousElement of
        MoveTo ->
            "M"

        LineTo ->
            "L"


svgViewBoxFromBoundingBox : BoundingBox2d -> String
svgViewBoxFromBoundingBox boundingBox =
    [ ( boundingBox |> BoundingBox2d.minX, boundingBox |> BoundingBox2d.minY ), boundingBox |> BoundingBox2d.dimensions ]
        |> List.concatMap tuple2ToList
        |> List.map String.fromFloat
        |> String.join " "


button : List (Html.Attribute event) -> List (Html.Html event) -> Html.Html event
button attributes =
    Html.button (attributes ++ [ HA.style "cursor" "pointer" ])
