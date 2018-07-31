module ParseSvg exposing (..)

import Point2d exposing (Point2d)
import Vector2d exposing (Vector2d)
import XmlParser
import Regex
import Tuple2
import Tuple4
import Maybe.Extra


type alias XmlElement =
    {   tag : String
    ,   attributes : List XmlParser.Attribute
    ,   children : List XmlParser.Node
    }

type alias VisualPolygon =
    {   points : List Point2d
    ,   fill : Maybe String
    ,   fillOpacity : Maybe String
    ,   stroke : Maybe String
    ,   strokeWidth : Maybe String
    }


xmlListSelfAndDescendantsNodesDepthFirst : XmlParser.Node -> List XmlParser.Node
xmlListSelfAndDescendantsNodesDepthFirst parent =
    case parent of
    XmlParser.Text _ -> []
    XmlParser.Element _ _ children ->
        [ parent ] ++ (children |> List.concatMap xmlListSelfAndDescendantsNodesDepthFirst)

xmlNodeAsElement : XmlParser.Node -> Maybe XmlElement
xmlNodeAsElement node =
    case node of
    XmlParser.Text _ -> Nothing
    XmlParser.Element tag attributes children -> Just { tag = tag, attributes = attributes, children = children }

xmlElementIsCircle : XmlElement -> Bool
xmlElementIsCircle xmlElement =
    (xmlElement.tag |> String.toLower) == "circle"

{-
2018-07-23 Example of circle from Gravit Designer:
<circle vector-effect="non-scaling-stroke" cx="0" cy="0" r="1" transform="matrix(5,0,0,5,5124.5,5115)" id="Node" fill="rgb(192,192,192)" fill-opacity="0.8"/>
-}
getCircleLocation : XmlElement -> Maybe Point2d
getCircleLocation circleElement =
    let
        maybeOffsetFromTransform =
            case circleElement |> getValueOfFirstAttributeWithMatchingNameIgnoringCasing "transform" of
            Nothing -> Just (Vector2d.fromComponents (0, 0))
            Just transform -> transform |> getOffsetFromSvgTransform

        parseCxCyResult =
            ("cx", "cy")
            |> Tuple2.mapBoth (\attributeName ->
                circleElement |> getValueOfFirstAttributeWithMatchingNameIgnoringCasing attributeName
                |> Maybe.map String.toFloat
                |> Maybe.withDefault (Ok 0))
    in
        case parseCxCyResult of
        (Ok cx, Ok cy) ->
            case maybeOffsetFromTransform of
            Just offsetFromTransform -> Just (Point2d.fromCoordinates (cx, cy) |> Point2d.translateBy offsetFromTransform)
            Nothing -> Nothing
        _ -> Nothing

getValueOfFirstAttributeWithMatchingNameIgnoringCasing : String -> XmlElement -> Maybe String
getValueOfFirstAttributeWithMatchingNameIgnoringCasing attributeName xmlElement =
    xmlElement.attributes
    |> List.filter (\attribute -> (attribute.name |> String.toLower) == (attributeName |> String.toLower))
    |> List.head |> Maybe.map .value

getOffsetFromSvgTransform : String -> Maybe Vector2d
getOffsetFromSvgTransform transform =
    case Regex.find Regex.All (Regex.regex "^matrix\\((.+)\\)$") transform of
    [ matrixMatch ] ->
        case matrixMatch.submatches |> List.head of
        Just (Just matrixComponents) ->
            case matrixComponents |> String.split "," of
            [ _, _, _, _, offsetX, offsetY ] ->
                case (offsetX |> String.toFloat, offsetY |> String.toFloat) of
                (Ok offsetX, Ok offsetY) -> Just (Vector2d.fromComponents (offsetX, offsetY))
                _ -> Nothing
            _ -> Nothing
        _ -> Nothing
    _ -> Nothing

{-
2018-07-23 Example of polygon in xml element from Gravit Designer:
<path d=" M 4583.5 4966 L 4663.5 5005 L 4654.5 5133 L 4477.5 5121 L 4378.5 5136 L 4469.5 4975 L 4583.5 4966 Z " id="Kerdis East" fill="rgb(216,181,143)"/>
-}
getVisualPolygonFromXmlElement : XmlElement -> Result String VisualPolygon
getVisualPolygonFromXmlElement xmlElement =
    case xmlElement |> getValueOfFirstAttributeWithMatchingNameIgnoringCasing "d" of
    Nothing -> Err "No path data"
    Just pathData ->
        case pathData |> polygonPointsFromSvgPathData of
        Err pathDataParseError -> Err ("Failed to map path data to polygon: " ++ pathDataParseError)
        Ok polygonPoints ->
            let
                (fill, fillOpacity, stroke, strokeWidth) =
                    ("fill","fill-opacity","stroke","stroke-width")
                    |> Tuple4.mapAll (\attributeName ->
                        xmlElement |> getValueOfFirstAttributeWithMatchingNameIgnoringCasing attributeName)
            in
                if (fill == Nothing) && (stroke == Nothing || strokeWidth == Nothing)
                then Err "No fill and no stroke"
                else Ok { points = polygonPoints, fill = fill, fillOpacity = fillOpacity, stroke = stroke, strokeWidth = strokeWidth }

polygonPointsFromSvgPathData : String -> Result String (List Point2d)
polygonPointsFromSvgPathData pathData =
    case Regex.find Regex.All (Regex.regex "^\\s*M([\\s\\d\\.L]+)Z\\s*$") pathData of
    [ polygonMatch ] ->
        case polygonMatch.submatches |> List.head of
        Just (Just pointsString) ->
            pointsString |> String.split "L"
            |> List.map (\pointString ->
                case pointString |> String.trim |> String.split " " of
                [ xString, yString ] ->
                    case (xString |> String.toFloat, yString |> String.toFloat) of
                    (Ok x, Ok y) -> Just (Point2d.fromCoordinates (x, y))
                    _ -> Nothing
                _ -> Nothing)
            |> Maybe.Extra.combine
            |> Result.fromMaybe "Failed to parse points"
        _ -> Err "No submatch in polygonMatch"
    _ -> Err "Does not match overall polygon path"

