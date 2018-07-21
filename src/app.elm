import GameWorld
import Html exposing (Html)
import Svg


type alias State =
    {   gameWorld : GameWorld.State
    }

type alias Event = ()


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
update event stateBefore = (stateBefore, Cmd.none)

subscriptions : State -> Sub Event
subscriptions state = Sub.none

view : State -> Html.Html a
view state =
    [ state.gameWorld |> GameWorld.view ]
    |> Svg.svg []

