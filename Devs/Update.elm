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
        SetYear ts -> ( { model | session = O.setSession model (Just (T.toYear T.utc ts)) Nothing (Just (DU.roundedDistance model.config.distance)), servicePlan = DU.getServicePlan } , Cmd.none )
        SetDistance dist ->
          let
            distance = Maybe.withDefault model.config.distance (String.toInt dist)
          in
            ( { model | config = O.setConfig model Nothing (Just distance), session = O.setSession model Nothing Nothing (Just (DU.roundedDistance distance)) }, Cmd.none )
        ToggleServicePlan -> ( { model | session = O.setSession model Nothing (Just (not model.session.showServicePlan)) Nothing }, Cmd.none )
