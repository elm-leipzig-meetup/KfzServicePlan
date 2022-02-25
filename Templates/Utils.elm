module Templates.Utils exposing ( .. )

import Html
import Html.Attributes as Attr
import Html.Events as Ev
import List.Extra as List
import UUID

import Devs.Objects as O
import Devs.TypeObject as TO
import Devs.Utils as DU
import UUID

getActionButton: String -> Bool -> TO.Msg -> Maybe String -> Html.Html TO.Msg
getActionButton label showBtn event titel =
  if showBtn then
    Html.input [
      Attr.type_ "button"
      , Attr.class "no-print"
      , Attr.value label
      , Attr.title (Maybe.withDefault "" titel)
      , Ev.onClick event
    ][]
  else Html.text ""

--getOption: String -> String -> String -> Html Msg
--getOption key sel label = Html.option [ Attr.value key, Attr.selected (key==sel) ][ Html.text label ]

getServiceApp: O.Model -> Html.Html TO.Msg
getServiceApp model =
  let
    content = if model.session.showKonfig
      then getKonfigForm model
      else getPlanDiv model
    confirmForm = getConfirmDiv model
    spEditForm = if model.session.showEditServicePlan
      then getSpEditForm (List.filter (\i -> i.uuid == Maybe.withDefault "" model.session.spForEdit) model.servicePlan |> List.head)
      else Html.text ""
  in
    Html.div [ Attr.class "appDiv" ] (List.append content [spEditForm, confirmForm])

getConfirmDiv: O.Model -> Html.Html TO.Msg
getConfirmDiv model =
  let
    stUuid = Maybe.withDefault Nothing (List.getAt 0 model.session.uuidForConfirmDelete)
    tdUuid = Maybe.withDefault Nothing (List.getAt 1 model.session.uuidForConfirmDelete)
    sUuid = Maybe.withDefault Nothing (List.getAt 2 model.session.uuidForConfirmDelete)
    event = if sUuid /= Nothing && tdUuid /= Nothing && stUuid /= Nothing then Just ((TO.GotServicePlanMsg << TO.RemoveStuff (Maybe.withDefault "" stUuid) (Maybe.withDefault "" tdUuid)) (Maybe.withDefault "" sUuid))
      else if sUuid == Nothing && tdUuid /= Nothing && stUuid /= Nothing then Just ((TO.GotServicePlanMsg << TO.RemoveTodo (Maybe.withDefault "" stUuid)) (Maybe.withDefault "" tdUuid))
      else if sUuid == Nothing && tdUuid == Nothing && stUuid /= Nothing then Just ((TO.GotServicePlanMsg << TO.RemoveServicePlan) (Maybe.withDefault "" stUuid))
      else Nothing
    msg = if sUuid /= Nothing && tdUuid /= Nothing && stUuid /= Nothing then "Soll der Materialeintrag gelöscht werden?"
      else if sUuid == Nothing && tdUuid /= Nothing && stUuid /= Nothing then "Soll der Arbeitsschritt gelöscht werden?"
      else if sUuid == Nothing && tdUuid == Nothing && stUuid /= Nothing then "Soll die Serviceaufgabe gelöscht werden?"
      else "Es gibt nichts zum Löschen!"
  in
    case event of
      Just e -> 
        Html.div [ Attr.class "formBG" ][
          Html.div [ Attr.class "confirmDiv" ] [
            Html.div[][ Html.text msg ]
            , getActionButton "Ok" True e Nothing
            , getActionButton "Abbrechen" True TO.HideConfirm Nothing
          ]
        ]
      Nothing -> Html.text ""

getFormDiv: Html.Html TO.Msg -> TO.Msg -> Html.Html TO.Msg
getFormDiv subForm event =
  Html.div [ Attr.class "formBG" ][
    Html.div [ Attr.class "formDiv" ] [
      subForm
      , Html.div[ Attr.class "formDivRow"][ getActionButton "Schließen" True event Nothing ]
    ]
  ]

getSpEditForm: Maybe O.ServicePlan -> Html.Html TO.Msg
getSpEditForm servicePlan =
  let
    sp = Maybe.withDefault O.getEmptyServicePlan servicePlan
    editForm = Html.div[][
        Html.div[][ Html.label[ Attr.for "years" ][ Html.text "Jahr(e):" ], Html.input[Attr.id "years", Attr.type_ "number", Attr.value (DU.getStringOrEmptyFromNumber sp.years), Ev.onInput (TO.GotServicePlanMsg << TO.SetYearInServicePlan sp.uuid)][] ]
        , Html.div[][ Html.label[ Attr.for "dist" ][ Html.text "Kilometer:" ], Html.input[Attr.id "dist", Attr.type_ "number", Attr.value (DU.getStringOrEmptyFromNumber sp.distance), Ev.onInput (TO.GotServicePlanMsg << TO.SetDistanceInServicePlan sp.uuid)][] ]
        , Html.div[](
          List.map (\td ->
            Html.div[][
              Html.div[][
                getActionButton "-" True (TO.ShowConfirm (Just sp.uuid, Just td.uuid, Nothing)) Nothing
                , Html.input[Attr.style "width" "93%", Attr.id ("todo" ++ td.uuid), Attr.value td.name, Ev.onInput (TO.GotServicePlanMsg << (TO.SetDotoName sp.uuid td.uuid))][]
                , Html.div[](
                  List.map (\stuff ->
                    Html.div[][
                      Html.span[ Attr.style "margin" "15px" ][]
                      , getActionButton "-" True (TO.ShowConfirm (Just sp.uuid, Just td.uuid, Just stuff.uuid)) Nothing
                      , Html.input[Attr.style "width" "89%", Attr.id ("stuff" ++ stuff.uuid), Attr.value stuff.name, Ev.onInput (TO.GotServicePlanMsg << TO.SetStuffName sp.uuid td.uuid stuff.uuid)][]
                    ]
                  ) td.stuff
                )
              ], Html.div[][ Html.span[ Attr.style "margin" "15px" ][], getActionButton "+" True ((TO.GotServicePlanMsg << TO.AddStuff sp.uuid) td.uuid) (Just "Neues Material hinzufügen") ]
            ]
          ) sp.todos
        )
        , Html.div[][ getActionButton "+" True ((TO.GotServicePlanMsg << TO.AddTodo) sp.uuid) (Just "Neue Aufgabe hinzufügen") ]
      ]
  in
    getFormDiv editForm (TO.ToggleEditServicePlan Nothing)

getKonfigForm: O.Model -> List (Html.Html TO.Msg)
getKonfigForm model = 
  [
    Html.div [ Attr.class "no-print" ][
      getActionButton "Speichern" True TO.ToggleKonfig Nothing
    ], Html.div [ Attr.class "no-print" ][
      Html.label [ Attr.for "year" ][ Html.text "Kaufjahr:" ]
      , Html.input [
        Attr.id "year"
        , Attr.type_ "number"
        , Attr.value (String.fromInt (model.config.buyingYear))
        , Ev.onInput TO.SetBuyingYear
      ][ ]
    ] , Html.div [ Attr.class "no-print" ][
      Html.label [ Attr.for "dist" ][ Html.text "Laufleistung (km):" ]
      , Html.input [
        Attr.id "dist"
        , Attr.type_ "number"
        , Attr.value (String.fromInt model.config.distance)
        , Ev.onInput TO.SetDistance
      ][ ]
    ]
--    , Html.div [][ getActionButton "+" True (TO.GotServicePlanMsg << TO.AddServicePlan) (Just "Neuen Serviceschritt hinzufügen") ]
    , Html.div [][ getActionButton "+" True ((TO.GotServicePlanMsg << TO.AddServicePlan) (UUID.toString UUID.nil)) (Just "Neuen Serviceschritt hinzufügen") ]
    , Html.div [ Attr.class "no-print, configListDiv" ][
      Html.ul [] (List.map showServiceList model.servicePlan)
    ]
  ]

showServiceList: O.ServicePlan -> Html.Html TO.Msg
showServiceList sp =
  Html.li [][
    if sp.years /= Nothing then Html.div[][ Html.text ("Jahre:" ++ DU.getStringOrEmptyFromNumber sp.years) ] else Html.text ""
    , if sp.distance /= Nothing then Html.div[][ Html.text ("Km:" ++ DU.getStringOrEmptyFromNumber sp.distance) ] else Html.text ""
    , Html.div[][ getActionButton "Bearbeiten" True (TO.ToggleEditServicePlan (Just sp.uuid)) Nothing, getActionButton "Löschen" True (TO.ShowConfirm (Just sp.uuid, Nothing, Nothing)) Nothing ]
    , Html.ul [] (List.map showServiceItem sp.todos)
  ]

getPlanDiv: O.Model -> List (Html.Html TO.Msg)
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
      getActionButton "Konfiguration" True TO.ToggleKonfig Nothing
      , getActionButton "Wartungsplan" (not model.session.showServicePlan) TO.ToggleServicePlan Nothing
    ], getServicePlan model
  ]

getServicePlan: O.Model -> Html.Html TO.Msg
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

showServiceItem: O.Todo -> Html.Html TO.Msg
showServiceItem todo =
  Html.li [][
    Html.text todo.name
    , Html.ul [ ] ( List.map (\item -> Html.li [][ Html.text item.name ]) todo.stuff )
  ]
