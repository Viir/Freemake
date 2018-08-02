module Console exposing (State, init, updateForChangedGame, updateForTimeProgress, applyCameraTransformToSvg)

import GameWorld
import Visuals
import Common exposing (..)
import Point2d exposing (Point2d)
import Vector2d exposing (Vector2d)
import Dict
import Tuple2
import Svg
import Svg.Attributes as SA


type alias State =
  { knownGame : GameWorld.State
  , cameraVelocity : Vector2d
  , cameraOffset : Vector2d
  }


init : GameWorld.State -> State
init game =
  { knownGame = game
  , cameraOffset = game |> defaultCameraOffsetFromGame |> Maybe.withDefault Vector2d.zero
  , cameraVelocity = Vector2d.zero
  }

applyCameraTransformToSvg : State -> Svg.Svg event -> Svg.Svg event
applyCameraTransformToSvg state =
  List.singleton >>
  (Svg.g [ SA.transform (Visuals.svgTransformTranslate (state.cameraOffset |> Vector2d.components |> Tuple2.mapBoth negate))])

defaultCameraOffsetFromGame : GameWorld.State -> Maybe Vector2d
defaultCameraOffsetFromGame game =
  let
    playerVisualLocation =
      case game.playerLocation of
      GameWorld.OnNode nodeId -> game.nodes |> Dict.get nodeId |> Maybe.map .visualLocation
  in
    playerVisualLocation
    |> Maybe.map (Point2d.coordinates >> Vector2d.fromComponents)

updateForChangedGame : GameWorld.State -> State -> State
updateForChangedGame gameState stateBefore = { stateBefore | knownGame = gameState }

updateForTimeProgress : Int -> State -> State
updateForTimeProgress = updateAnimateCamera

updateAnimateCamera : Int -> State -> State
updateAnimateCamera progressAmountMilli consoleBefore =
  let
    stepSizes =
      (animateCameraStepSizeMax |> List.repeat (progressAmountMilli // animateCameraStepSizeMax)) ++
      [ progressAmountMilli % animateCameraStepSizeMax ]
  in
    consoleBefore
    |> withListTransformApplied (stepSizes |> List.map updateAnimateCameraSingleStep)

animateCameraStepSizeMax : Int
animateCameraStepSizeMax = 5

cameraTransitionPanningDistanceThreshold : Float
cameraTransitionPanningDistanceThreshold = 1000

updateAnimateCameraSingleStep : Int -> State -> State
updateAnimateCameraSingleStep progressAmountMilli consoleBefore =
  if progressAmountMilli < 1
  then consoleBefore
  else
    let
      cameraDestinationOffset = (init consoleBefore.knownGame).cameraOffset

      progressSeconds = (progressAmountMilli |> toFloat) * 1e-3

      distanceToPan = cameraDestinationOffset |> Vector2d.difference consoleBefore.cameraOffset |> Vector2d.length

      (cameraOffset, cameraVelocity) =
        if not (distanceToPan <= cameraTransitionPanningDistanceThreshold)
        then (cameraDestinationOffset, Vector2d.zero)
        else
          let
            cameraVelocityAcceleratedToDestination =
              { offset = consoleBefore.cameraOffset, velocity = consoleBefore.cameraVelocity }
              |> accelerateCameraTowardsOffset (progressSeconds * 3) cameraDestinationOffset
              |> .velocity

            dampFactor = 0.9 ^ (progressSeconds * 30) 

            cameraVelocity = cameraVelocityAcceleratedToDestination |> Vector2d.scaleBy dampFactor

            cameraOffset = cameraVelocity |> Vector2d.scaleBy progressSeconds |> Vector2d.sum consoleBefore.cameraOffset
          in
            (cameraOffset, cameraVelocity)
    in
      { consoleBefore | cameraOffset = cameraOffset, cameraVelocity = cameraVelocity }

accelerateCameraTowardsOffset : Float -> Vector2d -> { offset: Vector2d, velocity : Vector2d } -> { velocity: Vector2d }
accelerateCameraTowardsOffset accelerationFactor cameraDestinationOffset cameraBefore =
  let
    offsetAcceleration = Vector2d.difference cameraDestinationOffset cameraBefore.offset |> Vector2d.scaleBy accelerationFactor
  in
    { velocity = cameraBefore.velocity |> Vector2d.sum offsetAcceleration }
