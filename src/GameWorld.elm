module GameWorld exposing (Node, State, init, view)

import Visuals
import Point2d exposing (Point2d)
import Dict
import Html
import Svg
import Svg.Attributes as SA


type Location = OnNode NodeId

type alias NodeId = Int

type alias Node =
    {   visualLocation : Point2d
    }

type alias State =
    {   playerLocation : Location
    ,   nodes : Dict.Dict NodeId Node
    }

nodes : Dict.Dict NodeId Node
nodes =
    [   { visualLocation = Point2d.fromCoordinates (100, 100) }
    ,   { visualLocation = Point2d.fromCoordinates (200, 100) }
    ]
    |> List.indexedMap (\i node -> (i, node))
    |> Dict.fromList

init : State
init =
    {   playerLocation = OnNode (nodes |> Dict.keys |> List.head |> Maybe.withDefault 0)
    ,   nodes = nodes
    }

view : State -> Svg.Svg a
view state =
    state.nodes |> Dict.toList |> List.map (nodeView state)
    |> Svg.g []

nodeView : State -> (NodeId, Node) -> Svg.Svg a
nodeView worldState (nodeId, node) =
    let
        isPlayerLocatedHere = worldState.playerLocation == (OnNode nodeId)

        nodeBaseView =
            Svg.circle [ SA.r "10", SA.fill "grey" ] []

        playerView =
            if isPlayerLocatedHere
            then Svg.circle [ SA.r "4", SA.fill "black" ] []
            else Html.text ""
    in
        [ nodeBaseView, playerView ]
        |> Svg.g [ SA.transform (node.visualLocation |> Point2d.coordinates |> Visuals.svgTransformTranslate) ]
