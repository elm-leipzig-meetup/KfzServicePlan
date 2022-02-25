port module Devs.Ports exposing (..)

import Devs.Objects exposing ( Config, ServicePlan )

port pushDataToStore: (Config, List ServicePlan, Bool) -> Cmd msg

port setDataFromStore: ((Int, Config, List ServicePlan) -> msg) -> Sub msg
