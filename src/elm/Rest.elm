module Rest exposing (..)

import HttpBuilder
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Json.Decode.Extra exposing (date)
import Json.Decode.Pipeline exposing (required, decode, optional)
import State
import Graph
import Number.Expanded
import Dict


server : String
server =
    "https://ssfak.duckdns.org/hme2"


modelsUrl : String
modelsUrl =
    server ++ "/models"


hyperModelsUrl : String
hyperModelsUrl =
    server ++ "/hypermodels"


type Models
    = Models (List State.Model)


type HyperModels
    = HyperModels (List State.Hypermodel)


type Version
    = Version String


type alias Msg a =
    Result Http.Error a


getResource : String -> Decode.Decoder a -> Cmd (Msg a)
getResource url decoder =
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson decoder)
        |> HttpBuilder.send identity


getModels : Cmd (Msg Models)
getModels =
    getResource modelsUrl (Decode.list modelDecoder |> Decode.map Models)


getHyperModels : Cmd (Msg HyperModels)
getHyperModels =
    getResource hyperModelsUrl (Decode.list hypermodelDecoder |> Decode.map HyperModels)


saveHyperModel : State.Hypermodel -> Cmd (Msg Version)
saveHyperModel hypermodel =
    HttpBuilder.post hyperModelsUrl
        |> HttpBuilder.withJsonBody (encodeHypermodel hypermodel)
        |> HttpBuilder.withExpect (Http.expectJson saveResponseDecoder)
        |> HttpBuilder.send identity


saveResponseDecoder : Decode.Decoder Version
saveResponseDecoder =
    Decode.at [ "version" ] Decode.string |> Decode.map Version


modelParamDecoder : Decode.Decoder State.ModelInOutput
modelParamDecoder =
    decode State.ModelInOutput
        |> required "name" Decode.string
        |> optional "is_dynamic" Decode.bool False
        |> required "data_type" Decode.string
        |> optional "unit" Decode.string ""
        |> optional "description" Decode.string ""
        |> optional "data_range" valueRangeDecoder Nothing


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
        |> optional "freezed" Decode.bool False
        |> required "inPorts" (Decode.list modelParamDecoder)
        |> required "outPorts" (Decode.list modelParamDecoder)
        |> required "perspectives" perspectiveDecoder


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
        |> required "version" Decode.int
        |> optional "canvas" Decode.string ""
        |> required "created_at" date
        |> required "updated_at" date
        |> optional "svgContent" Decode.string ""
        |> required "graph" Graph.graphDecoder


encodeHypermodel : State.Hypermodel -> Encode.Value
encodeHypermodel { title, id, description, canvas, graph, svgContent } =
    Encode.object
        [ ( "title", Encode.string title )
        , ( "uuid", Encode.string id )
        , ( "description", Encode.string description )
        , ( "canvas", Encode.string canvas )
        , ( "graph", Graph.encodeGraph graph )
        , ( "svg_content", Encode.string svgContent )
        ]
