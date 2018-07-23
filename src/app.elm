import GameWorld
import Visuals
import BoundingBox2d exposing (BoundingBox2d)
import Html exposing (Html)
import Html.Attributes as HA
import Svg
import Svg.Attributes as SA
import Dict


type alias State =
    {   gameWorld : GameWorld.State
    }

type Event = PlayerInput GameWorld.FromPlayerInput


main : Program Never State Event
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

init : (State, Cmd Event)
init = ({ gameWorld = GameWorld.init }, Cmd.none)

update : Event -> State -> (State, Cmd Event)
update event stateBefore =
    case event of
    PlayerInput playerInput ->
        ( { stateBefore | gameWorld = stateBefore.gameWorld |> GameWorld.updateForPlayerInput playerInput } , Cmd.none)

subscriptions : State -> Sub Event
subscriptions state = Sub.none

view : State -> Html.Html Event
view state =
    let
        viewbox =
            state.gameWorld.nodes |> Dict.values |> List.map .visualLocation
            |> BoundingBox2d.containingPoints
            |> Maybe.withDefault (BoundingBox2d.fromExtrema { minX = 0, minY = 0, maxX = 100, maxY = 100 })
            |> Visuals.svgViewBoxFromBoundingBox
    in
        [ state.gameWorld |> GameWorld.view |> Html.map PlayerInput ]
        |> Svg.svg [ SA.viewBox viewbox, HA.style [("width","100%"),("height","96vh")]]

