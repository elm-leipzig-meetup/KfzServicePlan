module Devs.TypeObject exposing (..)

import Time exposing (Posix)

import Devs.Objects exposing (..)

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
  | ShowConfirm (Maybe String, Maybe String, Maybe String)
  | HideConfirm
  | GotServicePlanMsg MsgSP

type MsgSP =
  AddServicePlan String
  | RemoveServicePlan String
  | SetYearInServicePlan String String
  | SetDistanceInServicePlan String String
  | SetDotoName String String String
  | SetStuffName String String String String
  | AddTodo String
  | AddStuff String String
  | RemoveTodo String String
  | RemoveStuff String String String
