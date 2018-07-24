import GameWorld
import Visuals
import BoundingBox2d exposing (BoundingBox2d)
import Html exposing (Html)
import Html.Attributes as HA
import Svg
import Svg.Attributes as SA


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
            BoundingBox2d.fromExtrema { minX = -300, minY = -200, maxX = 300, maxY = 200 }
            |> Visuals.svgViewBoxFromBoundingBox
    in
        [ state.gameWorld |> GameWorld.view |> Html.map PlayerInput ]
        |> Svg.svg [ SA.viewBox viewbox, HA.style [("width","100%"),("height","96vh")]]

