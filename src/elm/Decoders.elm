module Decoders exposing (modelParamDecoder, modelDecoder, hypermodelDecoder, experimentDecoder)

import State
import Json.Decode as Decode
import Json.Decode.Extra exposing (date)
import Json.Decode.Pipeline exposing (required, decode, optional)
import Number.Expanded
import Dict
import Graph


modelParamDecoder : Decode.Decoder State.ModelInOutput
modelParamDecoder =
    decode State.ModelInOutput
        |> required "id" Decode.int
        |> required "uuid" Decode.string
        |> required "name" Decode.string
        |> optional "is_dynamic" Decode.bool False
        |> optional "is_mandatory" Decode.bool True
        |> required "data_type" Decode.string
        |> optional "unit" Decode.string ""
        |> optional "description" Decode.string ""
        |> optional "data_range" valueRangeDecoder Nothing
        |> optional "default_value" (Decode.string |> Decode.nullable) Nothing
        |> required "semtype_array" (Decode.oneOf [ Decode.list Decode.string, Decode.succeed [] ])


valueRangeDecoder : Decode.Decoder (Maybe State.ValueRange)
valueRangeDecoder =
    let
        toExp s def =
            String.toFloat s
                |> Result.map Number.Expanded.Finite
                |> Result.withDefault def

        r s =
            case String.split "-" s of
                fst :: snd :: _ ->
                    Just
                        ( toExp fst Number.Expanded.NegInfinity
                        , toExp snd Number.Expanded.PosInfinity
                        )

                _ ->
                    Nothing
    in
        Decode.string |> Decode.map r


boolFromInt : Decode.Decoder Bool
boolFromInt =
    Decode.map
        (\i ->
            if i == 0 then
                False
            else
                True
        )
        Decode.int


modelDecoder : Decode.Decoder State.Model
modelDecoder =
    decode State.Model
        |> required "title" Decode.string
        |> required "id" Decode.int
        |> required "uuid" Decode.string
        |> optional "description" Decode.string ""
        |> optional "frozen" Decode.bool False
        |> required "inPorts" (Decode.list modelParamDecoder)
        |> required "outPorts" (Decode.list modelParamDecoder)
        |> required "perspectives" (Decode.oneOf [ perspectiveDecoder, Decode.null Dict.empty ])
        |> optional "usage" Decode.int 0
        |> optional "is_composite" Decode.bool False


perspectiveDecoder : Decode.Decoder (Dict.Dict String (List String))
perspectiveDecoder =
    let
        toPerspDict d =
            Dict.foldr
                (\k v li ->
                    (case k of
                        "perspective1" ->
                            ( .uri State.perspective1, v )

                        "perspective4" ->
                            ( .uri State.perspective4, v )

                        "perspective5" ->
                            ( .uri State.perspective5, v )

                        _ ->
                            ( k, v )
                    )
                        :: li
                )
                []
                d
                |> Dict.fromList
    in
        Decode.dict (Decode.list Decode.string)
            |> Decode.map toPerspDict


hypermodelDecoder : Decode.Decoder State.Hypermodel
hypermodelDecoder =
    decode State.Hypermodel
        |> required "title" Decode.string
        |> required "uuid" Decode.string
        |> optional "description" Decode.string ""
        |> required "version" Decode.string
        |> optional "canvas" Decode.string ""
        |> required "created_at" date
        |> required "updated_at" date
        |> optional "svgContent" Decode.string ""
        |> required "graph" Graph.graphDecoder
        |> optional "publishedRepoId" (Decode.maybe Decode.int) Nothing


experimentStatusDecoder : Decode.Decoder State.ExperimentStatus
experimentStatusDecoder =
    let
        f : String -> State.ExperimentStatus
        f s =
            if s == "RUNNING" then
                State.RUNNING
            else if s == "FINISHED_FAIL" then
                State.FINISHED_FAIL
            else if s == "FINISHED_OK" then
                State.FINISHED_OK
            else
                State.NOT_STARTED
    in
        Decode.string |> Decode.map f


experimentDecoder : Decode.Decoder State.Experiment
experimentDecoder =
    Decode.map6 State.Experiment
        (Decode.field "uuid" Decode.string)
        (Decode.field "hypermodel_uid" Decode.string)
        (Decode.field "id" Decode.int)
        (Decode.field "status" experimentStatusDecoder)
        (Decode.field "title" Decode.string)
        (Decode.field "version" Decode.int)
