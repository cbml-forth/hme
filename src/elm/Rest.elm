module Rest exposing (..)

import HttpBuilder
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Json.Decode.Pipeline exposing (required, decode, optional)
import State
import Graph


server : String
server =
    "https://ssfak.duckdns.org/hme"


modelsUrl : String
modelsUrl =
    server ++ "/models"


hyperModelsUrl : String
hyperModelsUrl =
    -- server ++ "/hypermodels"
    "https://ssfak.duckdns.org/hme2" ++ "/hypermodels"


type Models
    = Models (List State.Model)


type HyperModels
    = HyperModels (List State.Hypermodel)


type Version
    = Version String


type alias Msg a =
    Result Http.Error a



{--

type alias Return =
    Msg ServerResponseMsg

type alias HyperModelsResponse =
    Response HyperModels


type alias ModelsResponse =
    Response Models

type alias HyperModelSaveResponse =
    Response String --}


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
        |> optional "freezed" boolFromInt False
        |> required "inPorts" (Decode.list modelParamDecoder)
        |> required "outPorts" (Decode.list modelParamDecoder)


hypermodelDecoder : Decode.Decoder State.Hypermodel
hypermodelDecoder =
    decode State.Hypermodel
        |> required "title" Decode.string
        |> required "uuid" Decode.string
        |> optional "description" Decode.string ""
        |> required "version" Decode.int
        |> required "canvas" Decode.string
        |> required "graph" Graph.graphDecoder
        |> optional "svgContent" Decode.string ""


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
