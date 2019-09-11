module Devs.Utils exposing ( getSeed, getServicePlan, getActServicePlan, roundedDistance, getStringOrEmptyFromNumber)

import List.Extra as ListE exposing ( .. )
import Round as R
import Random

import Debug exposing (log)

import Devs.Objects as O exposing ( Model, ServicePlan, Todo )
import Devs.TypeObject as TO exposing ( Msg )

getSeed: O.Model -> Random.Seed
getSeed model =
  case model.session.currentSeed of
      Just seed ->  seed
      Nothing -> Random.initialSeed model.session.random

getStringOrEmptyFromNumber: Maybe Int -> String
getStringOrEmptyFromNumber nr = case nr of
  Just n -> String.fromInt n
  Nothing -> ""

getActServicePlan: Int -> Int -> List O.ServicePlan -> List O.Todo
getActServicePlan year dist plan =
  let
    filteredServicePlan = List.filter (isTodoForPlan year dist) plan
  in
    List.map getTodosFromServicePlan filteredServicePlan |> List.concat

isTodoForPlan: Int -> Int -> ServicePlan -> Bool
isTodoForPlan year dist sp =
  let
    yearFits = if sp.years /= Nothing then (modBy (Maybe.withDefault 0 sp.years) year == 0) else False
    distFits = if sp.distance /= Nothing then (modBy (Maybe.withDefault 0 sp.distance) dist == 0) else False
  in
    yearFits || distFits

roundedDistance: Int -> Int
roundedDistance dist =
  let
    length = (String.length (String.fromInt dist)) - 1
  in
    (Maybe.withDefault 0 (String.toInt (R.round -length (toFloat dist))))

getTodosFromServicePlan: ServicePlan -> List Todo
getTodosFromServicePlan sp =
  let
    info = if sp.distance /= Nothing then " (" ++ (String.fromInt (Maybe.withDefault 0 sp.distance)) ++ " km)"
      else if sp.years /= Nothing then " (" ++ (String.fromInt (Maybe.withDefault 0 sp.years)) ++ " Jahr(e))"
      else ""
  in
    List.map (\item -> { item | name = item.name ++ info }) sp.todos

getServicePlan: List ServicePlan
getServicePlan = [
    { years=Just 1
      , distance=Nothing
      , uuid="4fccc52f-58d9-4f5c-960f-4ce6d76d8721"
      , todos=[
        { name="Wechsel des Motorenöls und Filter"
          , uuid="195fb0ce-09c8-4238-bb27-4337faffeae9"
          , stuff = [
            {name="Ölfilter", uuid="d44924e7-af68-4122-9064-db9d1f5c3d3c"}, {name="8l 10W-50 Vollsynthetisch", uuid="b24d2f11-1e10-4475-b3de-f2c0c1a1b911"}
          ]
        }
      ]
    }, { years=Just 2
      , distance=Nothing
      , uuid="4b79b18b-1317-44b6-bfae-ff7b2e34ae71"
      , todos=[
        { name="Wechsel des Endantrieb-Öls"
          , uuid="63463c7f-2d3b-43ee-ae3f-827de6d9948e"
          , stuff = [
            {name="0-Ring 11,2x1,8", uuid="5f4c3d24-b801-4986-953e-ce132fc0a697"}
            , {name="D-Ring A12x16-CU", uuid="9c4a4c71-2f03-4eff-a088-c208f8e0f1e2"}
            , {name="0,2l Castrol Syntrax longlife 75WW-90 (Getriebeöl)", uuid="c75a81a0-61e6-444d-ab90-cc2790014690"}
          ]
        }, { name="Wechsel des Getriebeöls"
          , uuid="6bdae7f9-6808-4bbd-8e5f-48a0918b2cc8"
          , stuff = [
            {name="D-Ring A14x18-AL", uuid="2a370aaf-89ea-4c38-a481-687d0b120a35"}
            , {name="D-Ring A18x22", uuid="a32251e6-d999-42ba-88a8-141ef34c57c6"}
            , {name="0,7l Castrol Syntrax longlife 75WW-90 (Getriebeöl)", uuid="87e557de-a648-432e-b384-109426987623"}
          ]
        }, { name="Wechsel der Bremsflüssigkeit"
          , uuid="ac7be08c-46f1-4ebd-ac02-17aa54939a3a"
          , stuff = [{name="Bremsflüssigkeit", uuid="426b4bf8-6df6-422a-9d4f-e610993ed8f1"}] }
        , { name="ggf. Wechsel der Bremsbeläge"
          , uuid="8332072b-a241-48d5-b44f-1b560e78fd7c"
          , stuff = [{name="Organische Beläge für vorn (2x) und hinten (1x)", uuid="8b6ddc21-2a03-4680-bd5a-354dda846aea"}] }
      ]
    }, { years=Just 6
      , distance=Nothing
      , uuid="d4175f0b-e12d-4c4a-acf3-ff3053ffc84d"
      , todos=[
        { name="Wechsel des Lichtmaschinen-Keilriemen"
          , uuid="dfe9b629-90f2-4079-897d-fee86955f372"
          , stuff = [{name="Lichtmaschinen-Keilriemen", uuid="f440d721-f156-4935-9de7-4c12facded89"}]
        }, { name="Wechsel des Getriebeöls"
          , uuid="efe84e38-becd-4702-b6b9-e2d25977a7e4"
          , stuff = [
            {name="D-Ring A14x18-AL", uuid="e06e3784-a2a0-4cd0-8d72-ee14483e6363"}
            , {name="D-Ring A18x22", uuid="722c1592-5c40-4bf2-912e-f62166f0fabf"}
            , {name="0,7l Castrol Syntrax longlife 75WW-90 (Getriebeöl)", uuid="68268966-6509-42d9-a6e5-8a81d2ecfd18"}
          ]
        }
      ]
    }, { years=Nothing
      , distance=Just 10000
      , uuid="d09fa333-1801-4227-80eb-95d083630202"
      , todos=[
        { name="Kontrolle und Schmieren der Ständerzapfen", uuid="56437942-7e0c-4b8e-abd2-a3c1cc2c8a47", stuff = [] }
        , { name="Kontrolle der Lampen und Blinker", uuid="186c88b8-ae39-48c0-93ff-9e894169eef4", stuff = [] }
      ]
    }, { years=Nothing
      , distance=Just 20000
      , uuid="919a075e-9b60-4228-b061-c8ba62964b01"
      , todos=[
        { name="Wechsel der Zündkerzen"
          , uuid="07e08f43-80ae-46d2-bd8d-82f36550d59b"
          , stuff = [{name="4x NGK MAR8B-JDS", uuid="9d3e3447-9643-49d1-bdb1-1ce02042f150"}] }
        , { name="Wechsel des Luftfilters"
          , uuid="4b7c2235-4204-4f1a-befe-b0eecb16a505"
          , stuff = [{name="1x Luftfilter", uuid="6e6b02d6-de2e-4dde-93d5-3e78fc7426e5"}] }
      ]
    }
  ]
