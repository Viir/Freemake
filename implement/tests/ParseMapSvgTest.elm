module ParseMapSvgTest exposing (..)

import Expect
import ParseSvg
import Point2d exposing (Point2d)
import Test exposing (Test)
import XmlParser


parseTextNodes : Test
parseTextNodes =
    [ { name = "transform only in group node"
      , svg = """
<g transform="matrix(0.871,0,0,0.871,-612.951,-615.97)"> 
    <text transform="matrix(1,0,0,1,0,7.482)" id="place: mentoran-cave-exit" style="font-family:&quot;Open Sans&quot;;font-weight:400;font-size:7px;font-style:normal;fill:#000000;stroke:none;">place: mentoran-cave-exit</text> 
</g>
"""
      , expected =
            [ ( "place: mentoran-cave-exit", ( -612.951, -608.488 ) ) ]
      }
    , { name = "transform only in text node"
      , svg = """
<g clip-path="url(#_clipPath_jv5s8sgsuwORpxbKegK8So3o2PpwTI2H)"> 
    <text transform="matrix(0.871,0,0,0.871,-612.951,-609.455)" id="place: mentoran-cave-exit" style="font-family:'Open Sans';font-weight:400;font-size:7px;font-style:normal;font-variant-ligatures:none;fill:#000000;stroke:none;">place: mentoran-cave-exit</text> 
</g>
"""
      , expected =
            [ ( "place: mentoran-cave-exit", ( -612.951, -609.455 ) ) ]
      }
    ]
        |> List.map
            (\testCase ->
                (\_ ->
                    [ """<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" viewBox="0 0 10000 10000" width="10000pt" height="10000pt">"""
                    , testCase.svg
                    , """</svg>"""
                    ]
                        |> String.join "\n"
                        |> XmlParser.parse
                        |> Result.map
                            (.root
                                >> ParseSvg.xmlListSelfAndDescendantsNodesDepthFirst
                                >> List.filterMap ParseSvg.xmlNodeAsElement
                                >> List.filterMap ParseSvg.parseSvgTextWithLocation
                                >> List.map (Tuple.mapSecond Point2d.coordinates)
                            )
                        |> Expect.equal (Ok testCase.expected)
                )
                    |> Test.test testCase.name
            )
        |> Test.describe "parse text nodes"
