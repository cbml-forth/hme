module Uuid2 exposing (..)

import Random.Pcg


type Uuid
    = Uuid Int


toString : a -> String
toString =
    Basics.toString


uuidGenerator : Random.Pcg.Generator Uuid
uuidGenerator =
    Random.Pcg.int Random.Pcg.minInt Random.Pcg.maxInt |> Random.Pcg.map Uuid
