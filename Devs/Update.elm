module Devs.Update exposing (..)

import Time as T
import List.Extra as List
import UUID
import Random

--import Debug exposing (log)

import Devs.Ports as P

import Devs.Objects as O
import Devs.TypeObject as TO
import Devs.Utils as DU

-- Update
update : TO.Msg -> O.Model -> ( O.Model, Cmd TO.Msg )
update msg model =
    case msg of
        TO.NoOp -> ( model , Cmd.none)
        TO.NoOpStr _ -> ( model , Cmd.none)
        TO.NoOpInt _ -> ( model , Cmd.none)
        TO.ReadDataFromPublish (random, config, servicePlan) -> ( { model | config=config, servicePlan = if List.length servicePlan > 0 then servicePlan else DU.getServicePlan, session=O.setSession model Nothing Nothing Nothing Nothing Nothing Nothing (Just random) } , Cmd.none)
        TO.SetYear ts -> ( { model | session = O.setSession model (Just ((T.toYear T.utc ts))) Nothing Nothing Nothing (Just (DU.roundedDistance model.config.distance)) Nothing Nothing, servicePlan = DU.getServicePlan } , Cmd.none )
        TO.SetBuyingYear year  -> ( { model | config=O.setConfig model (String.toInt year) Nothing } , Cmd.none )
        TO.SetDistance dist ->
          let
            distance = Maybe.withDefault model.config.distance (String.toInt dist)
          in
            ( { model | config = O.setConfig model Nothing (Just distance), session = O.setSession model Nothing Nothing Nothing Nothing (Just (DU.roundedDistance distance)) Nothing Nothing }, Cmd.none )
        TO.ToggleServicePlan -> ( { model | session = O.setSession model Nothing (Just False) (Just (not model.session.showServicePlan)) Nothing Nothing Nothing Nothing }, Cmd.none )
        TO.ToggleKonfig -> (
            { model | session = O.setSession model Nothing (Just (not model.session.showKonfig)) (Just False) Nothing Nothing Nothing Nothing }
            , if model.session.showKonfig then P.pushDataToStore (model.config, model.servicePlan, False) else Cmd.none
          )
        TO.ToggleEditServicePlan spUuid ->
          let
            session = model.session
            newSession = {session | spForEdit=spUuid, showEditServicePlan = not session.showEditServicePlan }
          in
            ( { model | session =  newSession}, Cmd.none )
        TO.ShowConfirm ( spUuidMaybe, tdUuidMaybe, sUuidMaybe ) ->
          let
            session = model.session
          in
            ( { model | session = {session | uuidForConfirmDelete = [spUuidMaybe, tdUuidMaybe, sUuidMaybe] }}, Cmd.none )
        TO.HideConfirm ->
          let
            session = model.session
          in
            ( { model | session = {session | uuidForConfirmDelete = [] }}, Cmd.none )
        --TO.GotServicePlanMsg _ -> (model, Cmd.none)
        TO.GotServicePlanMsg subMsg -> updateWith TO.GotServicePlanMsg (updateServiceplan subMsg model)

updateWith : (subMsg -> TO.Msg) -> ( O.Model, Cmd subMsg ) -> ( O.Model, Cmd TO.Msg )
updateWith toMsg ( subModel, subCmd ) = ( subModel, Cmd.map toMsg subCmd )

updateServiceplan : TO.MsgSP -> O.Model -> ( O.Model, Cmd TO.MsgSP )
updateServiceplan msg model = 
  case msg of
    TO.AddServicePlan _ ->
      let
        ( newUuid, newSeed ) = Random.step UUID.generator (DU.getSeed model)
        session = model.session
        emptyServicePlan = O.getEmptyServicePlan
      in
        ( { model | servicePlan=List.append model.servicePlan [{emptyServicePlan | uuid = UUID.toString newUuid}], session = {session | currentSeed=Just newSeed } } , Cmd.none)
    TO.RemoveServicePlan uuid ->
      let
        session = model.session
      in
        ( { model | servicePlan=List.filter (\i -> i.uuid /= uuid) model.servicePlan, session = {session | uuidForConfirmDelete = [] } }, Cmd.none)
    TO.SetYearInServicePlan spUuid val ->
      let
        spForEdit = List.filter (\i -> i.uuid == spUuid) model.servicePlan |> List.head |> Maybe.withDefault O.getEmptyServicePlan
        newSpList = List.updateIf (\item -> item.uuid == spUuid) (\_ -> { spForEdit | years = String.toInt val}) model.servicePlan
      in
        ( { model | servicePlan=newSpList } , Cmd.none)
    TO.SetDistanceInServicePlan spUuid val ->
      let
        spForEdit = List.filter (\i -> i.uuid == spUuid) model.servicePlan |> List.head |> Maybe.withDefault O.getEmptyServicePlan
        newSpList = List.updateIf (\item -> item.uuid == spUuid) (\_ -> { spForEdit | distance = String.toInt val}) model.servicePlan
      in
        ( { model | servicePlan=newSpList } , Cmd.none)
    TO.SetDotoName spUuid tdUuid val ->
      let
        spForEdit = List.filter (\i -> i.uuid == spUuid) model.servicePlan |> List.head |> Maybe.withDefault O.getEmptyServicePlan
        newTodos = List.updateIf (\item -> item.uuid == tdUuid) (\item -> { item | name = val}) spForEdit.todos
        newSpList = List.updateIf (\item -> item.uuid == spUuid) (\_ -> { spForEdit | todos = newTodos}) model.servicePlan
      in
        ( { model | servicePlan=newSpList } , Cmd.none)
    TO.SetStuffName spUuid tdUuid sUuid val ->
      let
        spForEdit = List.filter (\i -> i.uuid == spUuid) model.servicePlan |> List.head |> Maybe.withDefault O.getEmptyServicePlan
        tdForEdit = List.filter (\i -> i.uuid == tdUuid) spForEdit.todos |> List.head |> Maybe.withDefault O.getEmptyTodo
        newStuffs = List.updateIf (\item -> item.uuid == sUuid) (\item -> { item | name = val}) tdForEdit.stuff
        newTodos = List.updateIf (\item -> item.uuid == tdUuid) (\_ -> { tdForEdit | stuff = newStuffs}) spForEdit.todos
        newSpList = List.updateIf (\item -> item.uuid == spUuid) (\_ -> { spForEdit | todos = newTodos}) model.servicePlan
      in
        ( { model | servicePlan=newSpList } , Cmd.none)
    TO.AddTodo spUuid ->
      let
        ( newUuid, newSeed ) = Random.step UUID.generator (DU.getSeed model)
        emptyTodo = O.getEmptyTodo
        newSpList = List.updateIf (\item -> item.uuid == spUuid) (\item -> { item | todos = List.append item.todos [{ emptyTodo | uuid = UUID.toString newUuid }]}) model.servicePlan
        session = model.session
      in
        ( { model | servicePlan=newSpList, session = {session | currentSeed=Just newSeed } } , Cmd.none)
    TO.AddStuff spUuid tdUuid ->
      let
        ( newUuid, newSeed ) = Random.step UUID.generator (DU.getSeed model)
        spForEdit = List.filter (\i -> i.uuid == spUuid) model.servicePlan |> List.head |> Maybe.withDefault O.getEmptyServicePlan
        emptyStuff = O.getEmptyStuff
        newTodos = List.updateIf (\item -> item.uuid == tdUuid) (\item -> { item | stuff = List.append item.stuff [{ emptyStuff | uuid = UUID.toString newUuid }]}) spForEdit.todos
        newSpList = List.updateIf (\item -> item.uuid == spUuid) (\_ -> { spForEdit | todos = newTodos}) model.servicePlan
        session = model.session
      in
        ( { model | servicePlan=newSpList, session = {session | currentSeed=Just newSeed } } , Cmd.none)
    TO.RemoveTodo spUuid tdUuid ->
      let
        session = model.session
        spForEdit = List.filter (\i -> i.uuid == spUuid) model.servicePlan |> List.head |> Maybe.withDefault O.getEmptyServicePlan
        newSpList = List.updateIf (\item -> item.uuid == spUuid) (\_ -> { spForEdit | todos = List.filter (\i -> i.uuid /= tdUuid) spForEdit.todos}) model.servicePlan
      in
        ( { model | servicePlan=newSpList, session = {session | uuidForConfirmDelete = [] } } , Cmd.none)
    TO.RemoveStuff spUuid tdUuid sUuid ->
      let
        session = model.session
        spForEdit = List.filter (\i -> i.uuid == spUuid) model.servicePlan |> List.head |> Maybe.withDefault O.getEmptyServicePlan
        newTodos = List.updateIf (\item -> item.uuid == tdUuid) (\item -> { item | stuff = List.filter (\i -> i.uuid /= sUuid) item.stuff}) spForEdit.todos
        newSpList = List.updateIf (\item -> item.uuid == spUuid) (\_ -> { spForEdit | todos = newTodos}) model.servicePlan
      in
        ( { model | servicePlan=newSpList, session = {session | uuidForConfirmDelete = [] } } , Cmd.none )
