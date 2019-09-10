module Devs.Update exposing (..)

import Time as T

import Debug exposing (log)

import Devs.Ports as P exposing (pushDataToStore)

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
        ReadDataFromPublish (random, config, servicePlan) -> ( { model | config=config, servicePlan = servicePlan, session=O.setSession model Nothing Nothing Nothing Nothing Nothing (Just random) } , Cmd.none)
        SetYear ts -> ( { model | session = O.setSession model (Just ((T.toYear T.utc ts))) Nothing Nothing (Just (DU.roundedDistance model.config.distance)) Nothing Nothing, servicePlan = DU.getServicePlan } , Cmd.none )
        SetBuyingYear year  -> ( { model | config=O.setConfig model (String.toInt year) Nothing } , Cmd.none )
        SetDistance dist ->
          let
            distance = Maybe.withDefault model.config.distance (String.toInt dist)
          in
            ( { model | config = O.setConfig model Nothing (Just distance), session = O.setSession model Nothing Nothing Nothing (Just (DU.roundedDistance distance)) Nothing Nothing }, Cmd.none )
        ToggleServicePlan -> ( { model | session = O.setSession model Nothing (Just False) (Just (not model.session.showServicePlan)) Nothing Nothing Nothing }, Cmd.none )
        ToggleKonfig -> (
            { model | session = O.setSession model Nothing (Just (not model.session.showKonfig)) (Just False) Nothing Nothing Nothing }
            , if model.session.showKonfig then P.pushDataToStore (model.config, model.servicePlan, False) else Cmd.none
          )
