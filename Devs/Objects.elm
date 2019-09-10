module Devs.Objects exposing (..)

import UUID exposing (UUID)
import Random

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
  , showKonfig: Bool
  , roundedDist: Int
  , random: Int
  , currentSeed: Maybe Random.Seed
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
  , showKonfig = False
  , roundedDist = 0
  , random=0
  , currentSeed=Nothing
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

setSession: Model -> Maybe Int -> Maybe Bool -> Maybe Bool -> Maybe Int -> Maybe Random.Seed -> Maybe Int -> Session
setSession model newYear newShowKonfig newShowServicePlan newRoundedDist newSeed newRandom =
  let
    currentSession = model.session
    cy = case newYear of
      Just y -> y
      Nothing -> currentSession.currentYear
    ssp = case newShowServicePlan of
      Just sp -> sp
      Nothing -> currentSession.showServicePlan
    sk = case newShowKonfig of
      Just k -> k
      Nothing -> currentSession.showKonfig
    rd = case newRoundedDist of
      Just d -> d
      Nothing -> currentSession.roundedDist
    seed = case newSeed of
      Just s -> Just s
      Nothing -> case newRandom of
          Just nr -> Just (Random.initialSeed nr)
          Nothing -> currentSession.currentSeed
    random = case newRandom of
      Just r -> r
      Nothing -> currentSession.random
  in
    { currentSession | currentYear = cy, showKonfig=sk, showServicePlan = ssp, roundedDist = rd, currentSeed=seed, random=random }
