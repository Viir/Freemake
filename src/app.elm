import GameWorld
import Console
import Visuals
import BoundingBox2d exposing (BoundingBox2d)
import Html exposing (Html)
import Html.Attributes as HA
import Svg
import Svg.Attributes as SA
import AnimationFrame


type alias State =
    {   gameWorld : GameWorld.State
    ,   console : Console.State
    }

type Event
    = PlayerInput GameWorld.FromPlayerInput
    | AnimationFrameDiff Float


main : Program Never State Event
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

init : (State, Cmd Event)
init = ({ gameWorld = GameWorld.init, console = Console.init GameWorld.init }, Cmd.none)

update : Event -> State -> (State, Cmd Event)
update event stateBefore =
    case event of
    PlayerInput playerInput ->
        let
            gameWorld = stateBefore.gameWorld |> GameWorld.updateForPlayerInput playerInput
            console = stateBefore.console |> Console.updateForChangedGame gameWorld
        in
            ( { stateBefore | gameWorld = gameWorld, console = console }, Cmd.none)
    AnimationFrameDiff diff ->
        ( { stateBefore | console = stateBefore.console |> Console.updateForTimeProgress (diff |> round) } , Cmd.none)

subscriptions : State -> Sub Event
subscriptions state = AnimationFrame.diffs AnimationFrameDiff

view : State -> Html.Html Event
view state =
    let
        viewbox =
            BoundingBox2d.fromExtrema { minX = -300, minY = -200, maxX = 300, maxY = 200 }
            |> Visuals.svgViewBoxFromBoundingBox

        gameWorldSvg =
            [ state.gameWorld |> GameWorld.view |> Html.map PlayerInput |> Console.applyCameraTransformToSvg state.console ]
            |> Svg.svg [ SA.viewBox viewbox, HA.style [("width","100%"),("height","96vh")]]
    in
        [ gameWorldSvg, productVersionOverlay ]
        |> Html.div [ HA.style [("font-family", Visuals.cssFontFamily)]]

productVersionOverlay : Html.Html e
productVersionOverlay =
    [ ("Freemake v" ++ productVersion) |> Html.text ]
    |> Html.div [ HA.style [("position","absolute"),("top","3px"),("margin","1px")]]

productVersion : String
productVersion = "2018-08-01"
