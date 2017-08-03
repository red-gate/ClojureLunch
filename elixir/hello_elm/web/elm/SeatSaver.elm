module SeatSaver exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)

type alias Seat =
  { seatNo : Int
  , occupied : Bool
  }

type alias Model = List Seat

main = view init

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
  ]

view m = ul [class "seats"] (List.map seatItem m)

seatItem seat = li [class "seat available"] [Html.text (toString seat.seatNo)]
