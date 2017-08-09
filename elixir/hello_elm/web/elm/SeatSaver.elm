module SeatSaver exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

import Task exposing (Task)

type alias Seat =
  { seatNo : Int
  , occupied : Bool
  }

type alias Model = List Seat

type Msg = Toggle Seat

main : Program Never Model Msg
main = program
  { init = init
  , update  = update
  , view = view
  , subscriptions = always <| Sub.none
}

port tasks : Signal (Task Never())
port tasks = app.tasks

init : (Model, Cmd Msg)
init = [ { seatNo = 1, occupied = False }
  , { seatNo = 2, occupied = False }
  , { seatNo = 3, occupied = False }
  , { seatNo = 4, occupied = False }
  , { seatNo = 5, occupied = False }
  , { seatNo = 6, occupied = False }
  , { seatNo = 7, occupied = False }
  , { seatNo = 8, occupied = False }
  , { seatNo = 9, occupied = False }
  , { seatNo = 10, occupied = False }
  , { seatNo = 11, occupied = False }
  , { seatNo = 12, occupied = False }
  ] ! []

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Toggle seatToToggle ->
      let updateSeat seatFromModel =
        if seatFromModel.seatNo == seatToToggle.seatNo
        then {seatFromModel | occupied = not seatFromModel.occupied}
        else seatFromModel
      in
        (List.map updateSeat model) ! []

view : Model -> Html Msg
view m = ul [class "seats"] (List.map seatItem m)

seatItem : Seat -> Html Msg
seatItem seat =
  let
    seatText = if seat.occupied
               then "occupied"
               else toString seat.seatNo
  in
    li
      [ class "seat available"
      , onClick (Toggle seat)
      ]
      [ text seatText ]
