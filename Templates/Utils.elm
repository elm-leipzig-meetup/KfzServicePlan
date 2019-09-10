module Templates.Utils exposing ( getServiceApp )

import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events as Ev exposing ( onClick, onInput )

import Devs.Objects as O exposing ( Model, ServicePlan )
import Devs.TypeObject as TO exposing ( .. )
import Devs.Utils as DU exposing ( getActServicePlan )

getActionButton: String -> Bool -> Msg -> Html Msg
getActionButton label showBtn event =
  if showBtn then
    Html.input [
      Attr.type_ "button"
      , Attr.class "no-print"
      , Attr.value label
      , Ev.onClick event
    ][]
  else Html.text ""

getOption: String -> String -> String -> Html Msg
getOption key sel label = Html.option [ Attr.value key, Attr.selected (key==sel) ][ Html.text label ]

getServiceApp: O.Model -> Html Msg
getServiceApp model =
  let
    content = if model.session.showKonfig
      then getKonfigForm model
      else getPlanDiv model
  in
    Html.div [ Attr.class "appDiv" ] content

getKonfigForm: O.Model -> List (Html Msg)
getKonfigForm model = [
    Html.div [ Attr.class "no-print" ][
      getActionButton "Schließen" True TO.ToggleKonfig
    ], Html.div [ Attr.class "no-print" ][
      Html.label [ Attr.for "year" ][ Html.text "Kaufjahr:" ]
      , Html.input [
        Attr.id "year"
        , Attr.type_ "number"
        , Attr.value (String.fromInt (model.config.buyingYear))
        , Ev.onInput TO.SetBuyingYear
      ][ ]
    ], Html.div [ Attr.class "no-print" ][
      Html.label [ Attr.for "dist" ][ Html.text "Laufleistung (km):" ]
      , Html.input [
        Attr.id "dist"
        , Attr.type_ "number"
        , Attr.value (String.fromInt model.config.distance)
        , Ev.onInput TO.SetDistance
      ][ ]
    ], Html.div [ Attr.class "no-print" ][
    ]
  ]

getPlanDiv: O.Model -> List (Html Msg)
getPlanDiv model = [
    Html.div [ Attr.class "no-print" ][
      Html.label [ Attr.for "age" ][ Html.text "Alter (Jahre):" ]
      , Html.input [
        Attr.id "age"
        , Attr.type_ "number"
        , Attr.disabled True
        , Attr.value (String.fromInt (model.session.currentYear - model.config.buyingYear))
      ][ ]
    ], Html.div [ Attr.class "no-print" ][
      Html.label [ Attr.for "dist" ][ Html.text "Laufleistung (km):" ]
      , Html.input [
        Attr.id "dist"
        , Attr.type_ "number"
        , Attr.disabled True
        , Attr.value (String.fromInt model.config.distance)
        , Ev.onInput TO.SetDistance
      ][ ]
    ], Html.div [][
      getActionButton "Konfiguration" True TO.ToggleKonfig
      , getActionButton "Wartungsplan" (not model.session.showServicePlan) TO.ToggleServicePlan
    ], getServicePlan model
  ]

getServicePlan: O.Model -> Html Msg
getServicePlan model =
  let
    serviceList = DU.getActServicePlan (model.session.currentYear - model.config.buyingYear) model.session.roundedDist model.servicePlan
  in
    if model.session.showServicePlan
      then Html.div [ Attr.class "planDiv" ] [
          Html.h2 [][ Html.text ("Serviceplan für Jahr " ++ String.fromInt model.session.currentYear ++ " und km " ++ String.fromInt model.session.roundedDist) ]
          , Html.ol [] (List.map showServiceItem serviceList)
        ]
      else Html.text ""

showServiceItem: O.Todo -> Html Msg
showServiceItem todo =
  Html.li [][
    Html.text todo.name
    , Html.ul [ ] ( List.map (\item -> Html.li [][ Html.text item ]) todo.stuff )
  ]
