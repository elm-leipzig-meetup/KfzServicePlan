port module ServiceR1200R exposing (..)

import Devs.Ports as P

import Browser exposing (..)
import Html exposing (..)
import Time as T
import Task

import Json.Decode as Json

import Devs.Objects as O exposing (Model, initialModel)
import Devs.Update as U exposing ( update )
import Devs.TypeObject as TO exposing ( .. )
import Templates.Utils as TU exposing ( getServiceApp )

import Debug exposing (log)

-- Methods

-- View
view : O.Model -> Html TO.Msg
view model = TU.getServiceApp model

main : Program () O.Model TO.Msg
main =
    Browser.element
        { init = \() -> init
        , view = view
        , update = U.update
        , subscriptions = subscriptions
        }

subscriptions : O.Model -> Sub TO.Msg
subscriptions model = Sub.batch [
    P.setDataFromStore TO.ReadDataFromPublish
  ]

init : ( O.Model, Cmd Msg )
init =  ( O.initialModel, Cmd.batch [
      P.pushDataToStore (initialModel.config, initialModel.servicePlan, True),
      Task.perform TO.SetYear T.now
    ]
  )
