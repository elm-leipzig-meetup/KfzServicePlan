module Devs.TypeObject exposing (..)

import Time exposing (Posix)

import Devs.Objects as O exposing (..)

-- Types

type Msg =
  NoOp
  | NoOpStr String
  | NoOpInt Int
  | ReadDataFromPublish (Int, Config, List ServicePlan)
  | SetYear Posix
  | SetDistance String
  | SetBuyingYear String
  | ToggleServicePlan
  | ToggleKonfig
