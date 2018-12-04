module Console exposing (State, applyCameraTransformToSvg, init, updateForChangedGame, updateForTimeProgress)

import Common exposing (..)
import Dict
import GameWorld
import Point2d exposing (Point2d)
import Svg
import Svg.Attributes as SA
import Vector2d exposing (Vector2d)
import Visuals


type alias State =
    { knownGame : GameWorld.State
    , cameraVelocity : Vector2d
    , cameraOffset : Vector2d
    , cameraFadeOut : Float
    }


init : GameWorld.State -> State
init game =
    { knownGame = game
    , cameraOffset = game |> defaultCameraOffsetFromGame |> Maybe.withDefault Vector2d.zero
    , cameraVelocity = Vector2d.zero
    , cameraFadeOut = 0
    }


applyCameraTransformToSvg : State -> Svg.Svg event -> Svg.Svg event
applyCameraTransformToSvg state viewedSvg =
    [ viewedSvg ]
        |> Svg.g [ SA.transform (Visuals.svgTransformTranslate (state.cameraOffset |> Vector2d.components |> tuple2MapAll negate)) ]


defaultCameraOffsetFromGame : GameWorld.State -> Maybe Vector2d
defaultCameraOffsetFromGame game =
    let
        playerVisualLocation =
            case game.playerLocation of
                GameWorld.OnNode nodeId ->
                    game.nodes |> Dict.get nodeId |> Maybe.map .visualLocation
    in
    playerVisualLocation
        |> Maybe.map (Point2d.coordinates >> Vector2d.fromComponents)


updateForChangedGame : GameWorld.State -> State -> State
updateForChangedGame gameState stateBefore =
    { stateBefore | knownGame = gameState }


updateForTimeProgress : Int -> State -> State
updateForTimeProgress =
    updateAnimateCamera


updateAnimateCamera : Int -> State -> State
updateAnimateCamera progressAmountMilli consoleBefore =
    let
        stepSizes =
            (animateCameraStepSizeMax |> List.repeat (progressAmountMilli // animateCameraStepSizeMax))
                ++ [ progressAmountMilli |> remainderBy animateCameraStepSizeMax ]
    in
    consoleBefore
        |> withListTransformApplied (stepSizes |> List.map updateAnimateCameraSingleStep)


animateCameraStepSizeMax : Int
animateCameraStepSizeMax =
    5


cameraTransitionPanningDistanceThreshold : Float
cameraTransitionPanningDistanceThreshold =
    1000


updateAnimateCameraSingleStep : Int -> State -> State
updateAnimateCameraSingleStep progressAmountMilli consoleBefore =
    if progressAmountMilli < 1 then
        consoleBefore

    else
        let
            cameraDestinationOffset =
                (init consoleBefore.knownGame).cameraOffset

            progressSeconds =
                (progressAmountMilli |> toFloat) * 1.0e-3

            dampFactor =
                0.9 ^ (progressSeconds * 30)

            distanceToPan =
                cameraDestinationOffset |> Vector2d.difference consoleBefore.cameraOffset |> Vector2d.length

            applyFadeOut =
                not (distanceToPan <= cameraTransitionPanningDistanceThreshold)

            ( cameraVelocityUpdatedForTransition, fadeOut ) =
                if applyFadeOut then
                    ( consoleBefore.cameraVelocity, consoleBefore.cameraFadeOut + progressSeconds )

                else
                    ( { offset = consoleBefore.cameraOffset, velocity = consoleBefore.cameraVelocity }
                        |> accelerateCameraTowardsOffset (progressSeconds * 3) cameraDestinationOffset
                        |> .velocity
                    , consoleBefore.cameraFadeOut - progressSeconds
                    )

            ( cameraVelocity, cameraOffset ) =
                if 1 <= fadeOut then
                    ( Vector2d.zero, cameraDestinationOffset )

                else
                    let
                        velocity =
                            cameraVelocityUpdatedForTransition |> Vector2d.scaleBy dampFactor
                    in
                    ( velocity, velocity |> Vector2d.scaleBy progressSeconds |> Vector2d.sum consoleBefore.cameraOffset )
        in
        { consoleBefore | cameraOffset = cameraOffset, cameraVelocity = cameraVelocity, cameraFadeOut = fadeOut |> clamp 0 1 }


accelerateCameraTowardsOffset : Float -> Vector2d -> { offset : Vector2d, velocity : Vector2d } -> { velocity : Vector2d }
accelerateCameraTowardsOffset accelerationFactor cameraDestinationOffset cameraBefore =
    let
        offsetAcceleration =
            Vector2d.difference cameraDestinationOffset cameraBefore.offset |> Vector2d.scaleBy accelerationFactor
    in
    { velocity = cameraBefore.velocity |> Vector2d.sum offsetAcceleration }
