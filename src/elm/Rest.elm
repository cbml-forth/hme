module Rest exposing (..)

import HttpBuilder
import Http
import Task
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
    "/hme2/api"


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


type HypoHyperModels
    = HypoHyperModels Models HyperModels


type Version
    = Version String String


type alias Msg a =
    Result Http.Error a


getResource : String -> Decode.Decoder a -> Http.Request a
getResource url decoder =
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson decoder)
        |> HttpBuilder.toRequest


sendRequest : Http.Request a -> Cmd (Msg a)
sendRequest req =
    Http.send identity req


getModels_ : Http.Request Models
getModels_ =
    getResource modelsUrl (Decode.list modelDecoder |> Decode.map Models)


getModels : Cmd (Msg Models)
getModels =
    sendRequest getModels_


getHyperModels_ : Http.Request HyperModels
getHyperModels_ =
    getResource hyperModelsUrl (Decode.list hypermodelDecoder |> Decode.map HyperModels)


getHyperModels : Cmd (Msg HyperModels)
getHyperModels =
    sendRequest getHyperModels_


getAllModels : Cmd (Msg HypoHyperModels)
getAllModels =
    let
        modelsTask =
            Http.toTask getModels_

        hypermodelsTask =
            Http.toTask getHyperModels_
    in
        Task.map2 HypoHyperModels modelsTask hypermodelsTask |> Task.attempt identity


saveHyperModel : State.Hypermodel -> Cmd (Msg Version)
saveHyperModel hypermodel =
    let
        etag =
            "\"" ++ hypermodel.version ++ "\""

        maybeAddMatchHeader =
            if String.isEmpty hypermodel.version then
                identity
            else
                HttpBuilder.withHeader "If-Match" etag

        decoder =
            saveResponseDecoder hypermodel.id
    in
        HttpBuilder.post hyperModelsUrl
            |> maybeAddMatchHeader
            |> HttpBuilder.withJsonBody (encodeHypermodel hypermodel)
            |> HttpBuilder.withExpect (Http.expectJson decoder)
            |> HttpBuilder.send identity


saveResponseDecoder : String -> Decode.Decoder Version
saveResponseDecoder hypermodelUuid =
    Decode.at [ "version" ] Decode.string |> Decode.map (Version hypermodelUuid)


modelParamDecoder : Decode.Decoder State.ModelInOutput
modelParamDecoder =
    decode State.ModelInOutput
        |> required "name" Decode.string
        |> optional "is_dynamic" Decode.bool False
        |> required "data_type" Decode.string
        |> optional "unit" Decode.string ""
        |> optional "description" Decode.string ""
        |> optional "data_range" valueRangeDecoder Nothing
        |> optional "defaultValue" (Decode.string |> Decode.nullable) Nothing


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
        |> optional "usage" Decode.int 0


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
