module Devs.TypeObject exposing (..)

import Time exposing (Posix)

import Devs.Objects as O exposing (..)

-- Types

type Msg =
  NoOp
  | NoOpStr String
  | NoOpInt Int
  | SetYear Posix
  | SetDistance String
  | ToggleServicePlan
