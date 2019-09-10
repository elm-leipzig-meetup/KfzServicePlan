port module Devs.Ports exposing (..)

import Devs.Objects as O exposing ( Config, ServicePlan )

port pushDataToStore: (Config, List ServicePlan, Bool) -> Cmd msg

port setDataFromStore: ((Int, Config, List ServicePlan) -> msg) -> Sub msg
