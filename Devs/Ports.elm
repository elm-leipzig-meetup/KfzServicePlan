port module Devs.Ports exposing (..)

import Devs.Objects as O exposing ( Model )

port getRandom: O.Model -> Cmd msg

port setRandom: (Int -> msg) -> Sub msg
