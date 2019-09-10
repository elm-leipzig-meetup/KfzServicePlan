module Devs.Objects exposing (..)

-- Model
type alias Model = {
  config: Config
  , session: Session
  , servicePlan: List ServicePlan
  }

type alias Config = {
  buyingYear: Int
  , distance: Int
  }

type alias Session = {
  currentYear: Int
  , showServicePlan: Bool
  , roundedDist: Int
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
  config = getInitialConfig
  , session = getEmptySession
  , servicePlan = []
  }

--Methods
getInitialConfig: Config
getInitialConfig = {
  buyingYear = 2012
  , distance = 58617
  }

getEmptySession: Session
getEmptySession = {
  currentYear = 0
  , showServicePlan = False
  , roundedDist = 0
  }

setConfig: Model -> Maybe Int -> Maybe Int -> Config
setConfig model newBuyingYear newDistance =
  let
    currentConfig = model.config
    by = case newBuyingYear of
      Just y -> y
      Nothing -> currentConfig.buyingYear
    dist = case newDistance of
      Just d -> d
      Nothing -> currentConfig.distance
  in
    { currentConfig | buyingYear = by, distance = dist }

setSession: Model -> Maybe Int -> Maybe Bool -> Maybe Int -> Session
setSession model newYear newShowServicePlan newRoundedDist =
  let
    currentSession = model.session
    cy = case newYear of
      Just y -> y
      Nothing -> currentSession.currentYear
    ssp = case newShowServicePlan of
      Just sp -> sp
      Nothing -> currentSession.showServicePlan
    rd = case newRoundedDist of
      Just d -> d
      Nothing -> currentSession.roundedDist
  in
    { currentSession | currentYear = cy, showServicePlan = ssp, roundedDist = rd }
