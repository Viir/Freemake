module Main exposing (main)

import BoundingBox2d
import Browser
import Browser.Events
import Console
import GameWorld
import Html
import Html.Attributes as HA
import Svg
import Svg.Attributes as SA
import Visuals exposing (HtmlStyle)


type alias State =
    { gameWorld : GameWorld.State
    , console : Console.State
    }


type Event
    = PlayerInput GameWorld.FromPlayerInput
    | AnimationFrameDiff Float


main : Program () State Event
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( State, Cmd Event )
init _ =
    ( { gameWorld = GameWorld.init, console = Console.init GameWorld.init }, Cmd.none )


update : Event -> State -> ( State, Cmd Event )
update event stateBefore =
    case event of
        PlayerInput playerInput ->
            let
                gameWorld =
                    stateBefore.gameWorld |> GameWorld.updateForPlayerInput playerInput

                console =
                    stateBefore.console |> Console.updateForChangedGame gameWorld
            in
            ( { stateBefore | gameWorld = gameWorld, console = console }, Cmd.none )

        AnimationFrameDiff diff ->
            ( { stateBefore | console = stateBefore.console |> Console.updateForTimeProgress (diff |> round) }, Cmd.none )


subscriptions : State -> Sub Event
subscriptions _ =
    Browser.Events.onAnimationFrameDelta AnimationFrameDiff


view : State -> Browser.Document Event
view state =
    let
        viewbox =
            BoundingBox2d.fromExtrema { minX = -300, minY = -200, maxX = 300, maxY = 200 }
                |> Visuals.svgViewBoxFromBoundingBox

        gameWorldSvg =
            [ state.gameWorld |> GameWorld.viewWorld |> Html.map PlayerInput |> Console.applyCameraTransformToSvg state.console ]
                |> Svg.svg [ SA.viewBox viewbox, HA.style "width" "100%", HA.style "height" "96vh" ]

        locationSpecific =
            [ state.gameWorld |> GameWorld.viewLocationSpecific |> Html.map PlayerInput ]
                |> Html.div [ HA.style "margin" "10px" ]
                |> List.singleton
                |> Html.div locationSpecificContainerStyle

        gameViewStyles =
            if 0 < state.console.cameraFadeOut then
                [ HA.style "filter" ("brightness(" ++ (((1 - state.console.cameraFadeOut) |> String.fromFloat) ++ ")")) ]

            else
                []

        gameView =
            [ gameWorldSvg, locationSpecific ]
                |> Html.div gameViewStyles
    in
    { body =
        [ [ gameView, productVersionOverlay ]
            |> Html.div [ HA.style "font-family" Visuals.cssFontFamily, HA.style "color" "whitesmoke" ]
        ]
    , title = "Freemake " ++ productVersion
    }


locationSpecificContainerStyle : HtmlStyle a
locationSpecificContainerStyle =
    [ ( "position", "absolute" )
    , ( "top", "0px" )
    , ( "margin", "10px" )
    , ( "width", "20%" )
    , ( "min-height", "10%" )
    , ( "background", "rgba(16,16,16,0.9)" )
    ]
        |> Visuals.htmlStyleFromList


productVersionOverlay : Html.Html e
productVersionOverlay =
    [ ("Freemake v" ++ productVersion) |> Html.text ]
        |> Html.div (Visuals.htmlStyleFromList [ ( "position", "absolute" ), ( "top", "0px" ), ( "right", "0px" ), ( "margin", "5px" ) ])


productVersion : String
productVersion =
    "2021-01-03"
