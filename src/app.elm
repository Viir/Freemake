import Html exposing (Html)

type alias State = ()

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
init = ((), Cmd.none)

update : Event -> State -> (State, Cmd Event)
update event stateBefore = (stateBefore, Cmd.none)

subscriptions : State -> Sub Event
subscriptions state = Sub.none

view : State -> Html.Html a
view state = Html.text "Hello World!"
