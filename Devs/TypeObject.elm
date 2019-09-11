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
  | ToggleEditServicePlan (Maybe String)
  | AddServicePlan
  | DelServicePlan String
  | SetYearInServicePlan String String
  | SetDistanceInServicePlan String String
  | SetDotoName String String String
  | SetStuffName String String String String
  | AddTodo String
  | AddStuff String String
  | RemoveTodo String String
  | RemoveStuff String String String
