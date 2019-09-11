module Devs.Update exposing (..)

import Time as T
import List.Extra as ListE
import UUID exposing (UUID)
import Random
import Task

import Debug exposing (log)

import Devs.Ports as P exposing (pushDataToStore)

import Devs.Objects as O exposing (..)
import Devs.TypeObject as TO exposing (..)
import Devs.Utils as DU exposing ( getServicePlan, roundedDistance, getSeed )

-- Update
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp -> ( model , Cmd.none)
        NoOpStr val -> ( model , Cmd.none)
        NoOpInt val -> ( model , Cmd.none)
        ReadDataFromPublish (random, config, servicePlan) -> ( { model | config=config, servicePlan = if List.length servicePlan > 0 then servicePlan else DU.getServicePlan, session=O.setSession model Nothing Nothing Nothing Nothing Nothing Nothing (Just random) } , Cmd.none)
        SetYear ts -> ( { model | session = O.setSession model (Just ((T.toYear T.utc ts))) Nothing Nothing Nothing (Just (DU.roundedDistance model.config.distance)) Nothing Nothing, servicePlan = DU.getServicePlan } , Cmd.none )
        SetBuyingYear year  -> ( { model | config=O.setConfig model (String.toInt year) Nothing } , Cmd.none )
        SetDistance dist ->
          let
            distance = Maybe.withDefault model.config.distance (String.toInt dist)
          in
            ( { model | config = O.setConfig model Nothing (Just distance), session = O.setSession model Nothing Nothing Nothing Nothing (Just (DU.roundedDistance distance)) Nothing Nothing }, Cmd.none )
        ToggleServicePlan -> ( { model | session = O.setSession model Nothing (Just False) (Just (not model.session.showServicePlan)) Nothing Nothing Nothing Nothing }, Cmd.none )
        ToggleKonfig -> (
            { model | session = O.setSession model Nothing (Just (not model.session.showKonfig)) (Just False) Nothing Nothing Nothing Nothing }
            , if model.session.showKonfig then P.pushDataToStore (model.config, model.servicePlan, False) else Cmd.none
          )
        ToggleEditServicePlan spUuid ->
          let
            session = model.session
            newSession = {session | spForEdit=spUuid, showEditServicePlan = not session.showEditServicePlan }
          in
            ( { model | session =  newSession}, Cmd.none )
        ShowConfirm ( spUuidMaybe, tdUuidMaybe, sUuidMaybe ) ->
          let
            session = model.session
          in
            ( { model | session = {session | uuidForConfirmDelete = [spUuidMaybe, tdUuidMaybe, sUuidMaybe] }}, Cmd.none )
        HideConfirm ->
          let
            session = model.session
          in
            ( { model | session = {session | uuidForConfirmDelete = [] }}, Cmd.none )
        AddServicePlan ->
          let
            ( newUuid, newSeed ) = Random.step UUID.generator (DU.getSeed model)
            session = model.session
          in
            ( { model | servicePlan=List.append model.servicePlan [{getEmptyServicePlan | uuid = UUID.toString newUuid}], session = {session | currentSeed=Just newSeed } } , Cmd.none)
        RemoveServicePlan uuid ->
          let
            session = model.session
          in
            ( { model | servicePlan=List.filter (\i -> i.uuid /= uuid) model.servicePlan, session = {session | uuidForConfirmDelete = [] } }, Cmd.none)
        SetYearInServicePlan spUuid val ->
          let
            spForEdit = List.filter (\i -> i.uuid == spUuid) model.servicePlan |> List.head |> Maybe.withDefault O.getEmptyServicePlan
            newSpList = ListE.updateIf (\item -> item.uuid == spUuid) (\item -> { spForEdit | years = String.toInt val}) model.servicePlan
          in
            ( { model | servicePlan=newSpList } , Cmd.none)
        SetDistanceInServicePlan spUuid val ->
          let
            spForEdit = List.filter (\i -> i.uuid == spUuid) model.servicePlan |> List.head |> Maybe.withDefault O.getEmptyServicePlan
            newSpList = ListE.updateIf (\item -> item.uuid == spUuid) (\item -> { spForEdit | distance = String.toInt val}) model.servicePlan
          in
            ( { model | servicePlan=newSpList } , Cmd.none)
        SetDotoName spUuid tdUuid val ->
          let
            spForEdit = List.filter (\i -> i.uuid == spUuid) model.servicePlan |> List.head |> Maybe.withDefault O.getEmptyServicePlan
            newTodos = ListE.updateIf (\item -> item.uuid == tdUuid) (\item -> { item | name = val}) spForEdit.todos
            newSpList = ListE.updateIf (\item -> item.uuid == spUuid) (\item -> { spForEdit | todos = newTodos}) model.servicePlan
          in
            ( { model | servicePlan=newSpList } , Cmd.none)
        SetStuffName spUuid tdUuid sUuid val ->
          let
            spForEdit = List.filter (\i -> i.uuid == spUuid) model.servicePlan |> List.head |> Maybe.withDefault O.getEmptyServicePlan
            tdForEdit = List.filter (\i -> i.uuid == tdUuid) spForEdit.todos |> List.head |> Maybe.withDefault O.getEmptyTodo
            newStuffs = ListE.updateIf (\item -> item.uuid == sUuid) (\item -> { item | name = val}) tdForEdit.stuff
            newTodos = ListE.updateIf (\item -> item.uuid == tdUuid) (\item -> { tdForEdit | stuff = newStuffs}) spForEdit.todos
            newSpList = ListE.updateIf (\item -> item.uuid == spUuid) (\item -> { spForEdit | todos = newTodos}) model.servicePlan
          in
            ( { model | servicePlan=newSpList } , Cmd.none)
        AddTodo spUuid ->
          let
            ( newUuid, newSeed ) = Random.step UUID.generator (DU.getSeed model)
            newSpList = ListE.updateIf (\item -> item.uuid == spUuid) (\item -> { item | todos = List.append item.todos [{ getEmptyTodo | uuid = UUID.toString newUuid }]}) model.servicePlan
            session = model.session
          in
            ( { model | servicePlan=newSpList, session = {session | currentSeed=Just newSeed } } , Cmd.none)
        AddStuff spUuid tdUuid ->
          let
            ( newUuid, newSeed ) = Random.step UUID.generator (DU.getSeed model)
            spForEdit = List.filter (\i -> i.uuid == spUuid) model.servicePlan |> List.head |> Maybe.withDefault O.getEmptyServicePlan
            newTodos = ListE.updateIf (\item -> item.uuid == tdUuid) (\item -> { item | stuff = List.append item.stuff [{ getEmptyStuff | uuid = UUID.toString newUuid }]}) spForEdit.todos
            newSpList = ListE.updateIf (\item -> item.uuid == spUuid) (\item -> { spForEdit | todos = newTodos}) model.servicePlan
            session = model.session
          in
            ( { model | servicePlan=newSpList, session = {session | currentSeed=Just newSeed } } , Cmd.none)
        RemoveTodo spUuid tdUuid ->
          let
            session = model.session
            spForEdit = List.filter (\i -> i.uuid == spUuid) model.servicePlan |> List.head |> Maybe.withDefault O.getEmptyServicePlan
            newSpList = ListE.updateIf (\item -> item.uuid == spUuid) (\item -> { spForEdit | todos = List.filter (\i -> i.uuid /= tdUuid) spForEdit.todos}) model.servicePlan
          in
            ( { model | servicePlan=newSpList, session = {session | uuidForConfirmDelete = [] } } , Cmd.none)
        RemoveStuff spUuid tdUuid sUuid ->
          let
            session = model.session
            spForEdit = List.filter (\i -> i.uuid == spUuid) model.servicePlan |> List.head |> Maybe.withDefault O.getEmptyServicePlan
            newTodos = ListE.updateIf (\item -> item.uuid == tdUuid) (\item -> { item | stuff = List.filter (\i -> i.uuid /= sUuid) item.stuff}) spForEdit.todos
            newSpList = ListE.updateIf (\item -> item.uuid == spUuid) (\item -> { spForEdit | todos = newTodos}) model.servicePlan
          in
            ( { model | servicePlan=newSpList, session = {session | uuidForConfirmDelete = [] } } , Cmd.none )
