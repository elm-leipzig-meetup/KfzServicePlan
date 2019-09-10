module Devs.Utils exposing ( getServicePlan, getActServicePlan, roundedDistance)

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
      , todos=[
        { name="Wechsel des Motorenöls und Filter", stuff = ["Ölfilter", "8l 10W-50 Vollsynthetisch"] }
      ]
    }, { years=Just 2
      , distance=Nothing
      , todos=[
        { name="Wechsel des Endantrieb-Öls"
          , stuff = ["0-Ring 11,2x1,8", "D-Ring A12x16-CU", "0,2l Castrol Syntrax longlife 75WW-90 (Getriebeöl)"]
        }, { name="Wechsel des Getriebeöls"
          , stuff = ["D-Ring A14x18-AL", "D-Ring A18x22", "0,7l Castrol Syntrax longlife 75WW-90 (Getriebeöl)"]
        }, { name="Wechsel der Bremsflüssigkeit", stuff = ["Bremsflüssigkeit"] }
        , { name="ggf. Wechsel der Bremsbeläge", stuff = ["Organische Beläge für vorn (2x) und hinten (1x)"] }
      ]
    }, { years=Just 6
      , distance=Nothing
      , todos=[
        { name="Wechsel des Lichtmaschinen-Keilriemen"
          , stuff = ["Lichtmaschinen-Keilriemen"]
        }, { name="Wechsel des Getriebeöls"
          , stuff = ["D-Ring A14x18-AL", "D-Ring A18x22", "0,7l Castrol Syntrax longlife 75WW-90 (Getriebeöl)"]
        }
      ]
    }, { years=Nothing
      , distance=Just 10000
      , todos=[
        { name="Kontrolle und Schmieren der Ständerzapfen", stuff = [] }
        , { name="Kontrolle der Lampen und Blinker", stuff = [] }
      ]
    }, { years=Nothing
      , distance=Just 20000
      , todos=[
        { name="Wechsel der Zündkerzen", stuff = ["4x NGK MAR8B-JDS"] }
        , { name="Wechsel des Luftfilters", stuff = ["1x Luftfilter"] }
      ]
    }
  ]
