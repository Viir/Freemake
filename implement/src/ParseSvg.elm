module ParseSvg exposing
    ( VisualPolygon
    , XmlElement
    , getCircleLocation
    , getOffsetFromSvgTransform
    , getValueOfFirstAttributeWithMatchingNameIgnoringCasing
    , getVisualPolygonFromXmlElement
    , parseSvgTextWithLocation
    , xmlElementIsCircle
    , xmlListSelfAndDescendantsNodesDepthFirst
    , xmlNodeAsElement
    )

import Common exposing (..)
import Maybe.Extra
import Point2d exposing (Point2d)
import Regex
import Vector2d exposing (Vector2d)
import XmlParser


type alias XmlElement =
    { tag : String
    , attributes : List XmlParser.Attribute
    , children : List XmlParser.Node
    }


type alias VisualPolygon =
    { points : List Point2d
    , fill : Maybe String
    , fillOpacity : Maybe String
    , stroke : Maybe String
    , strokeWidth : Maybe String
    }


xmlListSelfAndDescendantsNodesDepthFirst : XmlParser.Node -> List XmlParser.Node
xmlListSelfAndDescendantsNodesDepthFirst parent =
    case parent of
        XmlParser.Text _ ->
            []

        XmlParser.Element _ _ children ->
            [ parent ] ++ (children |> List.concatMap xmlListSelfAndDescendantsNodesDepthFirst)



{-
   2018-07-23 Example of circle from Gravit Designer:
   <circle vector-effect="non-scaling-stroke" cx="0" cy="0" r="1" transform="matrix(5,0,0,5,5124.5,5115)" id="Node" fill="rgb(192,192,192)" fill-opacity="0.8"/>
-}


getCircleLocation : XmlElement -> Maybe Point2d
getCircleLocation circleElement =
    let
        maybeOffsetFromTransform =
            circleElement |> offsetFromSvgElementTransformMatrix

        parseCxCyResult =
            ( "cx", "cy" )
                |> tuple2MapAll
                    (\attributeName ->
                        circleElement
                            |> getValueOfFirstAttributeWithMatchingNameIgnoringCasing attributeName
                            |> Maybe.map String.toFloat
                            |> Maybe.withDefault (Just 0)
                    )
    in
    case parseCxCyResult of
        ( Just cx, Just cy ) ->
            case maybeOffsetFromTransform of
                Just offsetFromTransform ->
                    Just (Point2d.fromCoordinates ( cx, cy ) |> Point2d.translateBy offsetFromTransform)

                Nothing ->
                    Nothing

        _ ->
            Nothing


getValueOfFirstAttributeWithMatchingNameIgnoringCasing : String -> XmlElement -> Maybe String
getValueOfFirstAttributeWithMatchingNameIgnoringCasing attributeName xmlElement =
    xmlElement.attributes
        |> List.filter (\attribute -> (attribute.name |> String.toLower) == (attributeName |> String.toLower))
        |> List.head
        |> Maybe.map .value



{-
   2018-08-01 Example from Gravit Designer:
   <g transform="matrix(1,0,0,1,5334,5017.105)">
       <text transform="matrix(1,0,0,1,0,8.551)" id="Text" style="font-family:&quot;Open Sans&quot;;font-weight:400;font-size:8px;font-style:normal;fill:#000000;stroke:none;">place: mentoran-cave-entrance</text>
   </g>
-}


parseSvgTextWithLocation : XmlElement -> Maybe ( String, Point2d )
parseSvgTextWithLocation xmlElement =
    case xmlElement.children |> List.filterMap xmlNodeAsElement |> List.filter xmlElementIsText of
        [ singleTextElement ] ->
            let
                offsets =
                    [ xmlElement, singleTextElement ]
                        |> List.filterMap offsetFromSvgElementTransformMatrix

                aggregateOffset =
                    List.foldl Vector2d.sum Vector2d.zero offsets

                text =
                    singleTextElement.children
                        |> List.filterMap xmlNodeAsText
                        |> String.concat
            in
            Just ( text, aggregateOffset |> Vector2d.components |> Point2d.fromCoordinates )

        _ ->
            Nothing


getOffsetFromSvgTransform : String -> Maybe Vector2d
getOffsetFromSvgTransform transform =
    case Regex.find ("^matrix\\((.+)\\)$" |> Regex.fromString |> Maybe.withDefault Regex.never) transform of
        [ matrixMatch ] ->
            case matrixMatch.submatches |> List.head of
                Just (Just matrixComponents) ->
                    case matrixComponents |> String.split "," of
                        [ _, _, _, _, offsetXString, offsetYString ] ->
                            case ( offsetXString, offsetYString ) |> tuple2MapAll String.toFloat of
                                ( Just offsetX, Just offsetY ) ->
                                    Just (Vector2d.fromComponents ( offsetX, offsetY ))

                                _ ->
                                    Nothing

                        _ ->
                            Nothing

                _ ->
                    Nothing

        _ ->
            Nothing



{-
   2018-07-23 Example of polygon in xml element from Gravit Designer:
   <path d=" M 4583.5 4966 L 4663.5 5005 L 4654.5 5133 L 4477.5 5121 L 4378.5 5136 L 4469.5 4975 L 4583.5 4966 Z " id="Kerdis East" fill="rgb(216,181,143)"/>

   2018-08-01 Example of polygon in xml element from Gravit Designer:
   <path d=" M -1523 -171 L -154.484 -171 L -246.744 -1185 L -1343.607 -1185 L -1523 -171 Z " id="Mentoran-Cave-Base" fill="rgb(47,47,47)"/>
-}


getVisualPolygonFromXmlElement : XmlElement -> Result String VisualPolygon
getVisualPolygonFromXmlElement xmlElement =
    case xmlElement |> getValueOfFirstAttributeWithMatchingNameIgnoringCasing "d" of
        Nothing ->
            Err "No path data"

        Just pathData ->
            case pathData |> polygonPointsFromSvgPathData of
                Err pathDataParseError ->
                    Err ("Failed to map path data to polygon: " ++ pathDataParseError)

                Ok polygonPoints ->
                    let
                        ( ( fill, fillOpacity ), ( stroke, strokeWidth ) ) =
                            ( ( "fill", "fill-opacity" ), ( "stroke", "stroke-width" ) )
                                |> tuple2MapAll
                                    (tuple2MapAll
                                        (\attributeName ->
                                            xmlElement |> getValueOfFirstAttributeWithMatchingNameIgnoringCasing attributeName
                                        )
                                    )
                    in
                    if (fill == Nothing) && (stroke == Nothing || strokeWidth == Nothing) then
                        Err "No fill and no stroke"

                    else
                        Ok { points = polygonPoints, fill = fill, fillOpacity = fillOpacity, stroke = stroke, strokeWidth = strokeWidth }


polygonPointsFromSvgPathData : String -> Result String (List Point2d)
polygonPointsFromSvgPathData pathData =
    case Regex.find ("^\\s*M([\\-\\s\\d\\.L]+)Z\\s*$" |> Regex.fromString |> Maybe.withDefault Regex.never) pathData of
        [ polygonMatch ] ->
            case polygonMatch.submatches |> List.head of
                Just (Just pointsString) ->
                    pointsString
                        |> String.split "L"
                        |> List.map
                            (\pointString ->
                                case pointString |> String.trim |> String.split " " of
                                    [ xString, yString ] ->
                                        case ( xString |> String.toFloat, yString |> String.toFloat ) of
                                            ( Just x, Just y ) ->
                                                Just (Point2d.fromCoordinates ( x, y ))

                                            _ ->
                                                Nothing

                                    _ ->
                                        Nothing
                            )
                        |> Maybe.Extra.combine
                        |> Result.fromMaybe "Failed to parse points"

                _ ->
                    Err "No submatch in polygonMatch"

        _ ->
            Err ("Path data Does not match overall polygon path: " ++ pathData)


offsetFromSvgElementTransformMatrix : XmlElement -> Maybe Vector2d
offsetFromSvgElementTransformMatrix xmlElement =
    case xmlElement |> getValueOfFirstAttributeWithMatchingNameIgnoringCasing "transform" of
        Nothing ->
            Just (Vector2d.fromComponents ( 0, 0 ))

        Just transform ->
            transform |> getOffsetFromSvgTransform


xmlElementIsCircle : XmlElement -> Bool
xmlElementIsCircle xmlElement =
    (xmlElement.tag |> String.toLower) == "circle"


xmlElementIsText : XmlElement -> Bool
xmlElementIsText xmlElement =
    (xmlElement.tag |> String.toLower) == "text"


xmlNodeAsElement : XmlParser.Node -> Maybe XmlElement
xmlNodeAsElement node =
    case node of
        XmlParser.Text _ ->
            Nothing

        XmlParser.Element tag attributes children ->
            Just { tag = tag, attributes = attributes, children = children }


xmlNodeAsText : XmlParser.Node -> Maybe String
xmlNodeAsText node =
    case node of
        XmlParser.Text text ->
            Just text

        XmlParser.Element tag attributes children ->
            Nothing
