module Devs.Update exposing (..)

import Time as T

import Debug exposing (log)

import Devs.Ports as P exposing (getRandom)

import Devs.Objects as O exposing (..)
import Devs.TypeObject as TO exposing (..)
import Devs.Utils as DU exposing ( getServicePlan, roundedDistance )

-- Update
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp -> ( model , Cmd.none)
        NoOpStr val -> ( model , Cmd.none)
        NoOpInt val -> ( model , Cmd.none)
        SetYear ts -> ( { model | currentYear = (T.toYear T.utc ts), servicePlan = DU.getServicePlan, roundedDist = DU.roundedDistance model.distance } , Cmd.none )
        SetDistance dist ->
          let
            distance = Maybe.withDefault model.distance (String.toInt dist)
          in
            ( { model | distance = distance, roundedDist = DU.roundedDistance distance }, Cmd.none )
        ToggleServicePlan -> ( { model | showServicePlan = not model.showServicePlan }, Cmd.none )
