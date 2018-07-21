module GameWorld exposing (Node, State, FromPlayerInput, init, updateForPlayerInput, view)

import Visuals
import Point2d exposing (Point2d)
import Dict
import Html
import Pointer
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

type FromPlayerInput = MoveToNode NodeId


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

updateForPlayerInput : FromPlayerInput -> State -> State
updateForPlayerInput playerInput stateBefore =
    case playerInput of
    MoveToNode destNodeId ->
        if stateBefore.nodes |> Dict.member destNodeId
        then { stateBefore | playerLocation = OnNode destNodeId }
        else stateBefore

view : State -> Svg.Svg FromPlayerInput
view state =
    state.nodes |> Dict.toList |> List.map (nodeView state)
    |> Svg.g []

nodeView : State -> (NodeId, Node) -> Svg.Svg FromPlayerInput
nodeView worldState (nodeId, node) =
    let
        isPlayerLocatedHere = worldState.playerLocation == (OnNode nodeId)

        nodeBaseView =
            Svg.circle [ SA.r "10", SA.fill "grey" ] []

        playerView =
            if isPlayerLocatedHere
            then Svg.circle [ SA.r "4", SA.fill "black" ] []
            else Html.text ""

        transformAttribute = SA.transform (node.visualLocation |> Point2d.coordinates |> Visuals.svgTransformTranslate)

        inputAttribute = Pointer.onDown (always (MoveToNode nodeId))
    in
        [ nodeBaseView, playerView ]
        |> Svg.g [ inputAttribute, transformAttribute ]
