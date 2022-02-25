module ServiceR1200R exposing (..)

import Devs.Ports as P

import Browser exposing (..)
import Html exposing (..)
import Time as T
import Task

import Devs.Objects as O
import Devs.Update as U
import Devs.TypeObject as TO
import Templates.Utils as TU

--import Debug exposing (log)

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
subscriptions _ = Sub.batch [
    P.setDataFromStore TO.ReadDataFromPublish
  ]

init : ( O.Model, Cmd TO.Msg )
init =  ( O.initialModel, Cmd.batch [
      P.pushDataToStore (O.initialModel.config, O.initialModel.servicePlan, True),
      Task.perform TO.SetYear T.now
    ]
  )
