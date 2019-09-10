module Devs.Objects exposing (..)

-- Model
type alias Model = {
  currentYear: Int
  , buyingYear: Int
  , distance: Int
  , roundedDist: Int
  , servicePlan: List ServicePlan
  , showServicePlan: Bool
  }

type alias ServicePlan = {
  years: Maybe Int
  , distance: Maybe Int
  , todos: List Todo
  }

type alias Todo = {
  name: String
  , stuff: List String
  }

--Model
initialModel: Model
initialModel = {
  currentYear = 1977
  , buyingYear = 2012
  , distance = 58617
  , roundedDist = 0
  , servicePlan = []
  , showServicePlan = False
  }
